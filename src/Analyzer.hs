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
                KeliSymType _            -> 2
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
                    if any 
                        (\func -> 
                            all (\(t1,t2) -> t1 `V.typeEquals` t2) 
                            (zip (funcParamTypes f) (funcParamTypes func))) fs then
                        Left (KErrorDuplicatedFunc f)
                    else
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
                                "int"   -> Right (KeliSymType (V.TypeAlias [id] V.TypeInt))
                                "str"   -> Right (KeliSymType (V.TypeAlias [id] V.TypeString))
                                "float" -> Right (KeliSymType (V.TypeAlias [id] V.TypeFloat))
                                "type"  -> Right (KeliSymType (V.TypeAlias [id] V.TypeType))
                                other   -> error("Unkown primitive type: " ++ other)
                        else if id' == "_primitive_constraint" then
                            case snd id of
                                "any" -> Right (KeliSymTypeConstraint id V.ConstraintAny)

                                other -> error ("Unknown primitive constraint type: " ++ other)
                        else 
                            continueAnalyzeConstDecl
            
            _ -> 
                continueAnalyzeConstDecl

        where 
            continueAnalyzeConstDecl = do
                -- insert temporary types into symtab to allow declaraion of recursive types
                let updatedSymtab = symtab |> (snd id, KeliSymType (V.TypeAlias [id] V.TypeSelf)) 
                result <- typeCheckExpr updatedSymtab CanBeAnything expr
                case result of
                    First typeCheckedExpr ->
                        Right (KeliSymConst id typeCheckedExpr)

                    -- if is tag union types, need to insert tags into symbol table
                    Second (V.TypeTagUnion _ tags) ->
                        let tagnames = map V.tagnameOf tags in
                        case findDuplicates tagnames of
                            Just duplicates -> 
                                Left (KErrorDuplicatedTags duplicates)
                            Nothing ->
                                let 
                                    -- circular structure. Refer https://wiki.haskell.org/Tying_the_Knot
                                    tagUnionType = (V.TypeTagUnion id tags')
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
                                    Right (KeliSymType (V.TypeAlias [id] tagUnionType))
                        
                    -- other types
                    Second type' ->
                        Right (KeliSymType (V.TypeAlias [id] type'))

                    Third tag ->
                        let
                            tagUnionType =(V.TypeTagUnion id [tag'])
                            tag' = case tag of
                                        V.CarrylessTag tagname _          -> 
                                            (V.CarrylessTag tagname tagUnionType)
                                        V.CarryfulTag tagname carryType _ -> 
                                            let carryType' = substituteSelfType tagUnionType carryType in
                                            (V.CarryfulTag tagname carryType' tagUnionType)
                                in
                            Right (KeliSymType (V.TypeAlias [id] tagUnionType))


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
            verifiedGenericParams <- verifyParamType symtab genericParams verifyTypeParam

            -- 0.2 populate symbol table with implicit type params
            symtab2 <- 
                foldM 
                    (\acc (id, typeparam) -> 
                        if member (snd id) acc then
                            Left (KErrorDuplicatedId [id])
                        else 
                            Right (acc |> (snd id, KeliSymImplicitTypeParam id typeparam)))
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


            -- 3. check if user is declaring generic type (a.k.a type constructor)
            if (case verifiedReturnType of V.TypeUndefined -> False; _ -> True;) && verifiedReturnType `V.typeEquals` V.TypeType then
                -- 3.1 make sure every param has the type of type
                case find (\(_,paramType) -> not (case paramType of V.TypeType -> True; _ -> False)) verifiedFuncParams of
                    Just p -> 
                        Left (KErrorInvalidTypeConstructorParam p)
                    Nothing ->
                        -- insert the name of this user-defined type into the symbol table, this is necessary for recursive types to be analyzed properly
                        let typeId = concat (map snd funcIds) in
                        undefined
                        -- let symtab''' = symtab'' |> (typeId, KeliSymType (V.TypeTemporaryAliasForRecursiveType funcIds (length funcParams))) in
                        -- case convertExprToSymbol symtab''' funcBody funcIds of
                        --     Right (Right symbols) ->
                        --         Right symbols
                                
                        --     Right (Left expr) ->
                        --         Left (KErrorBodyOfGenericTypeIsNotTypeDeclaration expr)

                        --     Left err ->
                        --         Left err
            else do
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
                                if bodyType `V.typeEquals` verifiedReturnType then
                                    Right (KeliSymFunc [resultFunc])
                                else 
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
        

    other -> undefined

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