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
import Env
import TypeCheck
import Util
import Unify

analyze :: [Raw.Decl] -> Either [KeliError] [KeliSymbol]
analyze decls = 
    let (errors, finalEnv, _) = analyzeDecls initialEnv decls in
    let analyzedSymbols = extractSymbols finalEnv in

    -- sorting is necessary, so that the transpilation order will be correct
    -- Smaller number means will be transpiled first
    let sortedSymbols = sortOn (
            \x -> case x of 
                KeliSymTag _             -> 1
                KeliSymType{}            -> 2
                KeliSymTypeConstructor{} -> 2
                KeliSymFunc _            -> 3
                KeliSymConst _ _         -> 4
                KeliSymTypeConstraint {} -> 6
                KeliSymInlineExprs _     -> 7
            ) analyzedSymbols in

    if length errors > 0 then
        Left errors
    else
        Right sortedSymbols

extractSymbols :: Env -> [KeliSymbol]
extractSymbols env = map snd (assocs env)

data TypeDecl = 
    TypeDecl 
        [V.StringToken] -- type signature
        V.Type          -- type body

analyzeDecls 
    :: Env -- previous env
    -> [Raw.Decl] -- parsed input
    -> ([KeliError], Env, [KeliSymbol]) -- (accumulatedErrors, newSymtab, newSymbols)

analyzeDecls env decls = 
    let (finalErrors, finalSymtab, finalSymbols) = 
            foldl'
            ((\(errors, prevSymtab, prevSymbols) nextDecl1 -> 
                let (newErrors, newSymtab, newSymbols) = 
                        case analyzeDecl nextDecl1 prevSymtab of
                            Right analyzedSymbol -> 
                                case insertSymbolIntoEnv analyzedSymbol prevSymtab of
                                    Right newSymtab' -> 
                                        ([], newSymtab', [analyzedSymbol])
                                    Left err' -> 
                                        ([err'], prevSymtab, [analyzedSymbol]) 
                            Left err' -> 
                                ([err'], prevSymtab, []) in
                
                (errors ++ newErrors, newSymtab, prevSymbols ++ newSymbols)
            )::([KeliError], Env, [KeliSymbol]) -> Raw.Decl -> ([KeliError],Env, [KeliSymbol]))
            ([], env, [])
            decls in
    (finalErrors, finalSymtab, finalSymbols)


analyzeDecl :: Raw.Decl -> Env -> Either KeliError KeliSymbol
analyzeDecl decl env = case decl of
    Raw.ConstDecl Raw.Const {
        Raw.constDeclId=id,
        Raw.constDeclValue=expr
    } -> 
        case expr of
            Raw.Id s@(_,id') -> 
                let reservedConstants = [
                        "tags",
                        "record",
                        "interface",
                        "ffi",
                        "undefined",
                        "Int",
                        "Float",
                        "String"] in
                case find (\x -> x == snd id) reservedConstants of
                    Just _ ->
                        Left (KErrorCannotRedefineReservedConstant id)

                    Nothing ->
                        continueAnalyzeConstDecl
            
            _ -> 
                continueAnalyzeConstDecl

        where 
            continueAnalyzeConstDecl = do
                -- insert temporary types into env to allow declaraion of recursive types
                let updatedSymtab = env |> (snd id, KeliSymType V.TypeSelf)
                (_, result) <- typeCheckExpr (Context 0 updatedSymtab) CanBeAnything expr
                case result of
                    First typeCheckedExpr ->
                        Right (KeliSymConst id typeCheckedExpr)

                    Second (V.TypeAnnotCompound _ _ t) -> do
                        case t of
                            V.TypeRecord _ expectedPropTypePairs ->
                                Right (KeliSymType (V.TypeRecord (Just id) expectedPropTypePairs))

                            t@V.TypeTaggedUnion{} ->
                                Right (KeliSymType t)
                            
                            _ ->
                                undefined

                    Second (V.TypeAnnotSimple _ t) -> do
                        undefined

                    Third tag -> do
                        taggedUnionType <- linkTagsTogether id [] tag []
                        Right (KeliSymType (V.TypeTaggedUnion taggedUnionType))

    Raw.FuncDecl(Raw.Func {
        Raw.funcDeclGenericParams = genericParams,
        Raw.funcDeclIds           = funcIds,
        Raw.funcDeclParams        = funcParams,
        Raw.funcDeclReturnType    = returnType,
        Raw.funcDeclBody          = funcBody
    }) -> do 
            let ctx = Context 0 env

            -- 0.1 Verify implicit type params
            verifiedGenericParams <- mapM (verifyBoundedTypeVar ctx) genericParams

            -- 0.2 populate symbol table with implicit type params
            env1 <- 
                foldM 
                    (\acc (id, constraint) -> 
                        if member (snd id) acc then
                            Left (KErrorDuplicatedId [id])
                        else 
                            Right (acc |> (snd id, KeliSymType (V.BoundedTypeVar id constraint))))
                    env 
                    verifiedGenericParams

            -- 1.1 Verify annotated types of each func param
            verifiedFuncParams <-
                    mapM
                        (\(id, typeAnnot) -> do 
                            (_, verifiedTypeAnnotation) <- verifyTypeAnnotation (Context 0 env1) typeAnnot
                            return (id, verifiedTypeAnnotation)) 
                        funcParams

            -- 1.2 populate symbol table with function parameters
            env2 <- 
                foldM 
                    (\acc (id, typeAnnot) ->
                        if member (snd id) acc then
                            Left (KErrorDuplicatedId [id])
                        else
                            Right (acc |> (snd id, KeliSymConst id (V.Expr (V.Id id) (V.getTypeRef typeAnnot)))))
                env1 
                verifiedFuncParams
            
            -- 2. verify return type
            verifiedReturnType <- 
                (case returnType of 
                    Just t -> do
                        (_, verifiedTypeAnnot) <- verifyTypeAnnotation (Context 0 env1) t
                        Right (V.getTypeRef verifiedTypeAnnot)

                    Nothing ->
                        Right (V.TypeUndefined))


            -- 3. Insert function into symbol table first (to allow user to defined recursive function) 
            let verifiedGenericParams' = map (\(id, c) -> V.BoundedTypeVar id c) verifiedGenericParams
            let tempFunc = KeliSymFunc 
                    [V.Func {
                        V.funcDeclIds = funcIds,
                        V.funcDeclGenericParams = verifiedGenericParams',
                        V.funcDeclBody = V.Expr (V.StringExpr V.nullStringToken) (V.TypeUndefined), -- temporary body (useless)
                        V.funcDeclParams = verifiedFuncParams,
                        V.funcDeclReturnType = verifiedReturnType
                    }] 

            env3 <- insertSymbolIntoEnv tempFunc env2


            -- 4. type check the function body
            (_, result) <- typeCheckExpr (Context 0 env3) CanBeAnything funcBody
            case result of
                First typeCheckedBody ->
                    let bodyType = getType typeCheckedBody in
                    let resultFunc = V.Func {
                                    V.funcDeclIds = funcIds,
                                    V.funcDeclGenericParams = verifiedGenericParams',
                                    V.funcDeclBody = typeCheckedBody,
                                    V.funcDeclParams = verifiedFuncParams,
                                    V.funcDeclReturnType = verifiedReturnType
                                } in

                    -- 4. ensure body type adheres to return type
                    case verifiedReturnType of
                        -- if return type is not declared, the return type of this function is inferred as the type of the body
                        V.TypeUndefined ->
                            Right (KeliSymFunc [resultFunc {V.funcDeclReturnType = bodyType}])

                        _ -> do
                            case unify typeCheckedBody verifiedReturnType of
                                Left err ->
                                    Left err

                                Right _ ->
                                -- if body type match expected return types
                                    Right (KeliSymFunc [resultFunc])

                _ -> undefined
    
    Raw.IdlessDecl expr -> do
        (_, result) <- typeCheckExpr (Context 0 env) CanBeAnything expr
        case result of
            First checkedExpr ->
                Right (KeliSymInlineExprs [checkedExpr])

            Second typeAnnot -> 
                Left (KErrorCannotDeclareTypeAsAnonymousConstant typeAnnot)
            
            Third tags ->
                Left (KErrorCannotDeclareTagAsAnonymousConstant tags)

        
    r@(Raw.GenericTypeDecl typeConstructorName ids typeParams typeBody) -> do
        -- 1. verify all type params
        verifiedTypeParams <- mapM (verifyBoundedTypeVar (Context 0 env)) typeParams 

        -- 2. populate symbol table with type params
        env2 <- 
            foldM 
                (\acc (id, constraint) -> 
                    if member (snd id) acc then
                        Left (KErrorDuplicatedId [id])
                    else 
                        Right (acc |> (snd id, KeliSymType (V.BoundedTypeVar id constraint))))
                env 
                verifiedTypeParams

        

        -- 3. populate symbol table with this type constructor (to allow recursve definition)
        let verifiedTypeParams' = map (\(id, c) -> V.BoundedTypeVar id c) verifiedTypeParams
        env3 <-
            if member (snd typeConstructorName) env2 then
                Left (KErrorDuplicatedId [typeConstructorName])
            else
                Right (env2 |> (snd typeConstructorName, 
                    KeliSymTypeConstructor (V.TaggedUnion typeConstructorName ids [] verifiedTypeParams')))

        -- 4. type check the body
        (_, typeCheckedBody) <- (typeCheckExpr (Context 0 env3) StrictlyAnalyzingType typeBody) 
        case typeCheckedBody of
            Third tag -> do
                taggedUnionType <- linkTagsTogether typeConstructorName ids tag verifiedTypeParams'
                Right (KeliSymTypeConstructor taggedUnionType)

            _ ->
                undefined

    other -> undefined


-- this function is for performing tying the knots (for tagged union types)
linkTagsTogether :: V.StringToken -> [V.StringToken] -> [V.UnlinkedTag] -> [V.Type] -> Either KeliError V.TaggedUnion
linkTagsTogether taggedUnionName ids tags typeParams = 
    let tagnames = 
            map 
            (\t -> case t of
                V.UnlinkedCarrylessTag name -> name
                V.UnlinkedCarryfulTag name _ -> name) tags in

    case findDuplicates tagnames of
        Just duplicates -> 
            Left (KErrorDuplicatedTags duplicates)
        Nothing ->
            let 
                -- circular structure. Refer https://wiki.haskell.org/Tying_the_Knot
                tagUnionType = (V.TaggedUnion taggedUnionName ids tags' typeParams)


                tags' =
                    map 
                        (\x -> case x of
                            V.UnlinkedCarrylessTag tag          -> 
                                (V.CarrylessTag tag tagUnionType)
                            V.UnlinkedCarryfulTag tag carryTypes -> 
                                let carryType' = map (\(key, typeAnnot) ->
                                        (key, substituteSelfType ( (V.TypeTaggedUnion tagUnionType)) (V.getTypeRef typeAnnot))) 
                                        carryTypes in
                                (V.CarryfulTag tag carryType' tagUnionType)) 
                        tags
            in

            Right tagUnionType
            


-- substitute source into target
substituteSelfType :: V.Type -> V.Type -> V.Type
substituteSelfType source target =
    case target of
        V.TypeSelf ->
            source
    
        V.TypeRecord name propTypePairs ->
            let updatedPropTypePairs = 
                    map 
                        (\(prop, type') -> 
                            (prop, substituteSelfType source type'))
                        propTypePairs
            in  (V.TypeRecord name updatedPropTypePairs)

        _ ->
            target
