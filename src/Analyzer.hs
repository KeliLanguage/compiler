{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Analyzer where

import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered ((|>), assocs, member, lookup) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError
import Symbol
import TypeCheck
import Util

analyze :: [Raw.Decl] -> Either [KeliError] [KeliSymbol]
analyze decls = 
    let (errors, finalSymtab, _) = analyzeDecls emptyKeliSymTab decls in
    let analyzedSymbols = extractSymbols finalSymtab in

    -- sorting is necessary, so that the transpilation order will be correct
    -- Smaller number means will be transpiled first
    let sortedSymbols = sortOn (
            \x -> case x of 
                KeliSymTag _             -> 1
                KeliSymType{}            -> 2
                KeliSymTypeConstructor{} -> 2
                KeliSymFunc _            -> 3
                KeliSymConst _ _         -> 4
                KeliSymImplicitTypeParam {}      -> 5
                KeliSymTypeConstraint {} -> 6
                KeliSymInlineExprs _     -> 7
            ) analyzedSymbols in

    if length errors > 0 then
        Left errors
    else
        Right sortedSymbols

extractSymbols :: KeliSymTab -> [KeliSymbol]
extractSymbols symtab = map snd (assocs symtab)

data TypeDecl = 
    TypeDecl 
        [V.StringToken] -- type signature
        V.Type          -- type body

analyzeDecls 
    :: KeliSymTab -- previous symtab
    -> [Raw.Decl] -- parsed input
    -> ([KeliError], KeliSymTab, [KeliSymbol]) -- (accumulatedErrors, newSymtab, newSymbols)

analyzeDecls symtab decls = 
    let (finalErrors, finalSymtab, finalSymbols) = 
            foldl'
            ((\(errors, prevSymtab, prevSymbols) nextDecl1 -> 
                let (newErrors, newSymtab, newSymbols) = 
                        case analyzeDecl nextDecl1 prevSymtab of
                            Right analyzedSymbol -> 
                                case insertSymbolIntoSymtab analyzedSymbol prevSymtab of
                                    Right newSymtab' -> 
                                        ([], newSymtab', [analyzedSymbol])
                                    Left err' -> 
                                        ([err'], prevSymtab, [analyzedSymbol]) 
                            Left err' -> 
                                ([err'], prevSymtab, []) in
                
                (errors ++ newErrors, newSymtab, prevSymbols ++ newSymbols)
            )::([KeliError], KeliSymTab, [KeliSymbol]) -> Raw.Decl -> ([KeliError],KeliSymTab, [KeliSymbol]))
            ([], symtab, [])
            decls in
    (finalErrors, finalSymtab, finalSymbols)

insertSymbolIntoSymtab :: KeliSymbol -> KeliSymTab -> Either KeliError KeliSymTab
insertSymbolIntoSymtab symbol symtab =
    case symbol of 
        KeliSymFunc [f] -> 
            let funcid = (intercalate "$" (map snd (V.funcDeclIds f))) in
            let funcsWithSameName = lookup funcid symtab in
            let funcParamTypes = (\func -> map snd (V.funcDeclParams func)) in
            case funcsWithSameName of
                Just (KeliSymFunc fs) ->
                    -- TODO: check for duplicated function (not only same ids, but also same type for each params)
                    -- if any 
                    --     (\func -> 
                    --         all (\(t1,t2) -> t1 `V.typeCompares` t2) 
                    --         (zip (funcParamTypes f) (funcParamTypes func))) fs then
                    --     Left (KErrorDuplicatedFunc f)
                    -- else
                    Right (symtab |> (funcid, KeliSymFunc (f:fs)))
                
                Just _ ->
                    Left (KErrorDuplicatedId (V.funcDeclIds f))

                Nothing ->
                    Right (symtab |> (funcid, symbol))

        KeliSymInlineExprs exprs ->
            case lookup "@inline_exprs" symtab of
                Just (KeliSymInlineExprs exprs') ->
                    Right (symtab |> ("@inline_exprs", KeliSymInlineExprs (exprs' ++ exprs)))

                Just _ ->
                    error "shouldn't reach here"
                
                Nothing ->
                    Right (symtab |> ("@inline_exprs", KeliSymInlineExprs exprs))

        _ -> 
            let (key,ids) = V.getIdentifier symbol in
            if member key symtab then
                Left (KErrorDuplicatedId ids)
            else 
                Right (symtab |> (key, symbol))

analyzeDecl :: Raw.Decl -> KeliSymTab -> Either KeliError KeliSymbol
analyzeDecl decl symtab = case decl of
    Raw.ConstDecl Raw.Const {
        Raw.constDeclId=id,
        Raw.constDeclValue=expr
    } -> 
        case expr of
            Raw.Id s@(_,id') -> 
                let reservedConstants = [
                        "_primitive_type",
                        "tag",
                        "record",
                        "interface",
                        "ffi",
                        "undefined"] in
                case find (\x -> x == snd id) reservedConstants of
                    Just _ ->
                        Left (KErrorCannotRedefineReservedConstant id)

                    Nothing ->
                        if id' == "_primitive_type" then
                            case snd id of
                                "Int"   -> Right (KeliSymType (V.TypeAlias [id] V.TypeInt) [])
                                "String"-> Right (KeliSymType (V.TypeAlias [id] V.TypeString) [])
                                "Float" -> Right (KeliSymType (V.TypeAlias [id] V.TypeFloat) [])
                                "Type"  -> Right (KeliSymType (V.TypeAlias [id] V.TypeType) [])
                                _       -> Left (KErrorCannotDefineCustomPrimitiveType id)
                        else 
                            continueAnalyzeConstDecl
            
            _ -> 
                continueAnalyzeConstDecl

        where 
            continueAnalyzeConstDecl = do
                -- insert temporary types into symtab to allow declaraion of recursive types
                let updatedSymtab = symtab |> (snd id, KeliSymType (V.TypeAlias [id] V.TypeSelf) []) 
                result <- typeCheckExpr updatedSymtab CanBeAnything expr
                case result of
                    First typeCheckedExpr ->
                        Right (KeliSymConst id typeCheckedExpr)

                    Second type' -> do
                        tiedType <- tieType [id] type'
                        Right (KeliSymType (V.TypeAlias [id] tiedType) [])

                    Third tag -> do
                        tiedType <- tieType [id] (V.TypeTagUnion [] [tag])
                        Right (KeliSymType (V.TypeAlias [id] tiedType) [])

    Raw.FuncDecl(Raw.Func {
        Raw.funcDeclGenericParams = genericParams,
        Raw.funcDeclIds           = funcIds,
        Raw.funcDeclParams        = funcParams,
        Raw.funcDeclReturnType    = returnType,
        Raw.funcDeclBody          = funcBody
    }) -> do 
            let verifyParamType = 
                    (\tempSymtab params verify -> 
                        mapM
                            (\(id, paramType) -> do 
                                verifiedType <- verify tempSymtab paramType
                                return (id, verifiedType)) 
                            params)

            -- 0.1 Verify implicit type params
            verifiedGenericParams <- mapM (verifyTypeParam symtab) genericParams 

            -- 0.2 populate symbol table with implicit type params
            symtab2 <- 
                foldM 
                    (\acc t@(V.TypeParam id _) -> 
                        if member (snd id) acc then
                            Left (KErrorDuplicatedId [id])
                        else 
                            Right (acc |> (snd id, KeliSymImplicitTypeParam t)))
                    symtab 
                    verifiedGenericParams

            -- 1.1 Verify annotated types of each func param
            verifiedFuncParams <- verifyParamType symtab2 funcParams verifyType

            -- 1.2 populate symbol table with function parameters
            symtab3 <- 
                foldM 
                    (\acc (id, type') ->
                        if member (snd id) acc then
                            Left (KErrorDuplicatedId [id])
                        else
                            Right (acc |> (snd id, KeliSymConst id (V.Expr (V.Id id) type'))))
                symtab2 
                verifiedFuncParams
            
            -- 2. verify return type
            verifiedReturnType <- 
                (case returnType of 
                    Just returnType' ->
                        verifyType symtab3 returnType'
                    Nothing ->
                        Right V.TypeUndefined)


            -- 3. Insert function into symbol table first (to allow user to defined recursive function) 
            let tempFunc = KeliSymFunc 
                    [V.Func {
                        V.funcDeclIds = funcIds,
                        V.funcDeclGenericParams = verifiedGenericParams,
                        V.funcDeclBody = V.Expr (V.StringExpr V.nullStringToken) V.TypeUndefined, -- temporary body (useless)
                        V.funcDeclParams = verifiedFuncParams,
                        V.funcDeclReturnType = verifiedReturnType
                    }] 

            symtab4 <- insertSymbolIntoSymtab tempFunc symtab3


            -- 4. type check the function body
            result <- typeCheckExpr symtab4 CanBeAnything funcBody
            case result of
                First typeCheckedBody ->
                    let bodyType = getType typeCheckedBody in
                    let resultFunc = V.Func {
                                    V.funcDeclIds = funcIds,
                                    V.funcDeclGenericParams = verifiedGenericParams,
                                    V.funcDeclBody = typeCheckedBody,
                                    V.funcDeclParams = verifiedFuncParams,
                                    V.funcDeclReturnType = verifiedReturnType
                                } in

                    -- 4. ensure body type adheres to return type
                    case verifiedReturnType of
                        -- if return type is not declared, the return type of this function is inferred as the type of the body
                        V.TypeUndefined ->
                            Right (KeliSymFunc [resultFunc {V.funcDeclReturnType = bodyType}])

                        _ ->
                            case typeCompares symtab4 bodyType verifiedReturnType of
                                -- if body type match expected return types
                                Applicable True ->
                                    Right (KeliSymFunc [resultFunc])
                                
                                -- else
                                _ ->
                                    case bodyType of
                                        -- if the body is `undefined`, bypass the type checking
                                        V.TypeUndefined -> 
                                            Right (KeliSymFunc [resultFunc])

                                        _ -> 
                                            Left (KErrorUnmatchingFuncReturnType typeCheckedBody verifiedReturnType)
                
                _ -> undefined
    
    Raw.IdlessDecl expr -> do
        result <- typeCheckExpr symtab CanBeAnything expr
        case result of
            First checkedExpr ->
                Right (KeliSymInlineExprs [checkedExpr])

            Second type' -> 
                Left (KErrorCannotDeclareTypeAsAnonymousConstant type')
            
            Third tag ->
                Left (KErrorCannotDeclareTagAsAnonymousConstant tag)
        
    r@(Raw.GenericTypeDecl typeConstructorName ids typeParams typeBody) -> do
        -- 1. verify all type params
        verifiedTypeParams <- mapM (verifyTypeParam symtab) typeParams 

        -- 2. populate symbol table with type params
        symtab2 <- 
            foldM 
                (\acc t@(V.TypeParam id _) -> 
                    if member (snd id) acc then
                        Left (KErrorDuplicatedId [id])
                    else 
                        Right (acc |> (snd id, KeliSymExplicitTypeParam t)))
                symtab 
                verifiedTypeParams

        -- 3. populate symbol table with this type constructor (to allow recursve definition)
        symtab3 <-
            if member (snd typeConstructorName) symtab2 then
                Left (KErrorDuplicatedId [typeConstructorName])
            else
                Right (symtab2 |> (snd typeConstructorName, 
                    KeliSymTypeConstructor (V.TypeConstructor typeConstructorName ids verifiedTypeParams V.TypeUndefined)))

        -- 4. type check the body
        typeCheckedBody <- (typeCheckExpr symtab3 StrictlyAnalyzingType typeBody) >>= extractType >>= tieType [typeConstructorName]

        Right (KeliSymTypeConstructor (V.TypeConstructor typeConstructorName ids verifiedTypeParams typeCheckedBody))


        -- error (pTraceShow r $ "hello")
            -- -- 3. check if user is declaring generic type (a.k.a type constructor)
            -- if (case verifiedReturnType of V.TypeUndefined -> False; _ -> True;) && verifiedReturnType `V.typeCompares` V.TypeType then
            --     -- 3.1 make sure every param has the type of type
            --     case find (\(_,paramType) -> not (case paramType of V.TypeType -> True; _ -> False)) verifiedFuncParams of
            --         Just p -> 
            --             Left (KErrorInvalidTypeConstructorParam p)

            --         Nothing -> do
            --             let ids = funcIds

            --             verifiedTypeParams <- mapM (verifyTypeParam symtab) funcParams

            --             -- insert self name to allow recursive type definition 
            --             let updatedSymtab1 = symtab |> (intercalate "$" (map snd ids), KeliSymType (V.TypeAlias ids V.TypeSelf) verifiedTypeParams) 

            --             -- insert explicit type params into symtab
            --             let updatedSymtab2 =
            --                     foldl' 
            --                     (\tempSymtab t@(V.TypeParam (_,name) _) -> 
            --                         -- TODO: check for duplicates
            --                         tempSymtab |> (name, KeliSymExplicitTypeParam t))
            --                     updatedSymtab1
            --                     verifiedTypeParams

            --             typeBody <- typeCheckExpr updatedSymtab2 StrictlyAnalyzingType funcBody >>= extractType


            --             tieType (TypeDecl funcIds typeBody)
            --             -- insert the name of this user-defined type into the symbol table, this is necessary for recursive types to be analyzed properly
            --             -- let typeId = concat (map snd funcIds) in
            --             -- undefined
            --             -- let symtab''' = symtab'' |> (typeId, KeliSymType (V.TypeTemporaryAliasForRecursiveType funcIds (length funcParams))) in
            --             -- case convertExprToSymbol symtab''' funcBody funcIds of
            --             --     Right (Right symbols) ->
            --             --         Right symbols
                                
            --             --     Right (Left expr) ->
            --             --         Left (KErrorBodyOfGenericTypeIsNotTypeDeclaration expr)

            --             --     Left err ->
            --             --         Left err
            -- else do

    other -> undefined


-- this function is for performing tying the knots (for tagged union types)
tieType :: [V.StringToken] -> V.Type -> Either KeliError V.Type
tieType typeName typeBody = 
    case typeBody of
        -- if is tag union types, need to insert tags into symbol table
        (V.TypeTagUnion _ tags) ->
            let tagnames = map V.tagnameOf tags in
            case findDuplicates tagnames of
                Just duplicates -> 
                    Left (KErrorDuplicatedTags duplicates)
                Nothing ->
                    let 
                        -- circular structure. Refer https://wiki.haskell.org/Tying_the_Knot
                        tagUnionType = (V.TypeTagUnion typeName tags')
                        tags' =
                            map 
                                (\x -> case x of
                                    V.CarrylessTag tag _          -> 
                                        (V.CarrylessTag tag tagUnionType)
                                    V.CarryfulTag tag carryType _ -> 
                                        let carryType' = substituteSelfType tagUnionType carryType in
                                        (V.CarryfulTag tag carryType' tagUnionType)) 
                                tags
                    in
                    Right tagUnionType
            
        -- other types
        other ->
            Right other

substituteSelfType :: V.Type -> V.Type -> V.Type
substituteSelfType source target =
    case target of
        V.TypeSelf ->
            source
        
        V.TypeRecord propTypePairs ->
            let updatedPropTypePairs = 
                    map 
                        (\(prop, type') -> 
                            (prop, substituteSelfType source type'))
                        propTypePairs
            in V.TypeRecord updatedPropTypePairs
            

        _ ->
            target 
