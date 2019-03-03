{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Analyzer where

import Text.Parsec.Pos
import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered ((|>), assocs, member) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError
import Module
import Env
import TypeCheck
import Util
import Unify

analyze :: [(ModuleName,Env)] -> [Raw.Decl] -> ([KeliError], Env, [V.Decl])
analyze importedEnvs decls = 
    let (errors, env, analyzedDecls) = analyzeDecls importedEnvs emptyEnv decls in

    -- sorting is necessary, so that the transpilation order will be correct
    -- Smaller number means will be transpiled first
    let sortedDecls = sortOn (
                \x -> case x of 
                    V.TaggedUnionDecl{}    -> 1
                    V.ObjectAliasDecl{}    -> 2
                    V.FuncDecl{}           -> 3
                    V.ConstDecl{}          -> 4
                    V.IdlessDecl{}         -> 5
                ) (analyzedDecls) in

    (errors, env, sortedDecls)

extractSymbols :: Env -> [KeliSymbol]
extractSymbols env = map snd (assocs env)

data TypeDecl = 
    TypeDecl 
        [V.StringToken] -- type signature
        V.Type          -- type body

analyzeDecls 
    :: [(ModuleName,Env)] -- imported envs
    -> Env -- previous env
    -> [Raw.Decl] -- parsed input
    -> ([KeliError], Env, [V.Decl]) -- (accumulatedErrors, newEnv, symbols)

analyzeDecls importedEnvs env rawDecls = 
    let (errors, updatedEnv, _, analyzedDecls) = analyzeDecls' importedEnvs env rawDecls [] in
    (errors, updatedEnv, analyzedDecls)

-- this function will perform multipassing
-- so that declaration order will be insignificant
analyzeDecls' 
    :: [(ModuleName,Env)]          -- imported envs
    -> Env            -- current envs 
    -> [Raw.Decl]     -- input raw decls
    -> [V.Decl]       -- previous verified decls
    -> 
        ([KeliError], -- errors
        Env,          -- updated env
        [Raw.Decl],   -- declarations that failed to pass the type checker
        [V.Decl])     -- declarations that passed the type checker

analyzeDecls' importedEnvs env inputRawDecls prevVerifiedDecls =
    let (currentErrors, updatedEnv, failedDecls, currentVerifiedDecls) =
            foldl'
                ((\(errors, prevEnv, prevFailedDecls, prevPassedDecls) currentRawDecl -> 
                    let (newErrors, newEnv, newFailedDecls, newPassedDecls) = 
                            -- Do partial analyzation (to get function signature (if it's a function))
                            case analyzeDecl currentRawDecl prevEnv importedEnvs of
                                Right partiallyAnalyzedDecl -> 

                                    -- add the function signature into env
                                    -- this is to allow recursive (even mutually recursive) functio to be type checked
                                    let updatedPrevEnv = 
                                            case partiallyAnalyzedDecl of
                                                PaFuncDecl f _ ->
                                                    insertSymbolIntoEnv (KeliSymFunc [f]) prevEnv

                                                _ ->
                                                    Right prevEnv in

                                    case updatedPrevEnv of
                                        Right updatedPrevEnv' ->
                                            case analyzePaDecl partiallyAnalyzedDecl updatedPrevEnv' importedEnvs of
                                                Right analyzedDecl ->
                                                    case toSymbol analyzedDecl of
                                                        Just symbol ->
                                                            case insertSymbolIntoEnv symbol prevEnv of
                                                                Right newEnv' -> 
                                                                    ([], newEnv', [], [analyzedDecl])

                                                                Left err' -> 
                                                                    ([err'], updatedPrevEnv', [currentRawDecl], []) 

                                                        Nothing ->
                                                            ([], updatedPrevEnv', [], [analyzedDecl]) 

                                                Left err' -> 
                                                    ([err'], updatedPrevEnv', [currentRawDecl],[])

                                        Left err' -> 
                                            ([err'], prevEnv, [currentRawDecl],[])

                                Left err' -> 
                                    ([err'], prevEnv, [currentRawDecl],[]) in
                    
                    (errors ++ newErrors, 
                        newEnv, 
                        prevFailedDecls ++ newFailedDecls, 
                        prevPassedDecls ++ newPassedDecls)
                )::([KeliError], Env, [Raw.Decl], [V.Decl]) -> Raw.Decl -> ([KeliError],Env, [Raw.Decl], [V.Decl]))
                ([], env, [], [])
                inputRawDecls in

    -- if the number of failedDecls had decreased, continue multipassing
    if length failedDecls < length inputRawDecls then
        -- note that the current errors will be ignored
        analyzeDecls' importedEnvs updatedEnv failedDecls (prevVerifiedDecls ++ currentVerifiedDecls)

    -- else stop multipassing
    else
        (currentErrors, updatedEnv, failedDecls, prevVerifiedDecls ++ currentVerifiedDecls)


-- NOTE: Pa means Partially Analyzed
data PaDecl 
    = PaConstDecl 
        Raw.Const

    | PaFuncDecl
        V.FuncSignature
        Raw.Expr

    | PaIdlessDecl
        SourcePos
        Raw.Expr

    | PaGenericTypeDecl 
        Raw.StringToken     -- name
        [Raw.StringToken]   -- trailing ids
        [(Raw.StringToken, Raw.Expr)] -- type params
        Raw.Expr            -- type body

analyzeDecl :: Raw.Decl -> Env -> [(ModuleName,Env)] -> Either KeliError PaDecl
analyzeDecl rawDecl env importedEnvs = case rawDecl of
    Raw.ConstDecl c -> 
        Right (PaConstDecl c)
    
    Raw.IdlessDecl pos expr ->
        Right (PaIdlessDecl pos expr)

    Raw.GenericTypeDecl typeConstructorName ids typeParams typeBody ->
        Right (PaGenericTypeDecl typeConstructorName ids typeParams typeBody)

    Raw.FuncDecl(Raw.Func {
        Raw.funcDeclDocString     = docstring,
        Raw.funcDeclGenericParams = genericParams,
        Raw.funcDeclIds           = funcIds,
        Raw.funcDeclParams        = funcParams,
        Raw.funcDeclReturnType    = returnType,
        Raw.funcDeclBody          = funcBody
    }) -> do 
            let ctx = Context 0 env importedEnvs

            -- 1.0 Verify implicit type params
            verifiedGenericParams <- mapM (verifyBoundedTypeVar ctx) genericParams
            let verifiedGenericParams' = map (\(id, c) -> V.BoundedTypeVar id c) verifiedGenericParams

            -- 1.1 populate symbol table with implicit type params
            env1 <- 
                foldM 
                    (\acc (id, constraint) -> 
                        if member (snd id) acc then
                            Left (KErrorDuplicatedId [id])
                        else 
                            Right (acc |> (snd id, KeliSymType (V.BoundedTypeVar id constraint))))
                    env 
                    verifiedGenericParams

            -- 2 Verify annotated types of each func param
            verifiedFuncParams <-
                    mapM
                        (\(id, typeAnnot) -> do 
                            (_, verifiedTypeAnnotation) <- verifyTypeAnnotation (Context 0 env1 importedEnvs) typeAnnot
                            return (id, verifiedTypeAnnotation)) 
                        funcParams

            -- 3. verify return type
            verifiedReturnType <- 
                (case returnType of 
                    Just t -> do
                        (_, verifiedTypeAnnot) <- verifyTypeAnnotation (Context 0 env1 importedEnvs) t
                        Right (V.getTypeRef verifiedTypeAnnot)

                    Nothing ->
                        Right (V.TypeUndefined))



            -- 4. Return this function signature
            let funcSig = V.FuncSignature{
                        V.funcDeclDocString = docstring,
                        V.funcDeclIds = funcIds,
                        V.funcDeclGenericParams = verifiedGenericParams',
                        V.funcDeclParams = verifiedFuncParams,
                        V.funcDeclReturnType = verifiedReturnType
                    }

            Right (PaFuncDecl funcSig funcBody)

    
analyzePaDecl :: PaDecl -> Env -> [(ModuleName,Env)] -> Either KeliError V.Decl
analyzePaDecl paDecl env importedEnvs = case paDecl of
    PaConstDecl Raw.Const {
        Raw.constDeclId=id,
        Raw.constDeclValue=expr
    } -> 
        case expr of
            Raw.Id _ -> 
                let reservedConstants = [
                        "tags",
                        "object",
                        "interface",
                        "ffi",
                        "undefined",
                        "module",
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
                let updatedEnv = env |> (snd id, KeliSymType V.TypeSelf)
                (_, result) <- typeCheckExpr (Context 0 updatedEnv importedEnvs) CanBeAnything expr
                case result of
                    First typeCheckedExpr ->
                        Right (V.ConstDecl id typeCheckedExpr)

                    Second (V.TypeAnnotCompound _ _ t) -> do
                        case t of
                            V.TypeObject _ expectedPropTypePairs ->
                                Right (V.ObjectAliasDecl id expectedPropTypePairs)

                            V.TypeTaggedUnion taggedUnion ->
                                Right (V.TaggedUnionDecl taggedUnion)
                            
                            _ ->
                                undefined

                    Second (V.TypeAnnotSimple _ t) -> do
                        undefined

                    Third tag -> do
                        taggedUnionType <- linkTagsTogether id [] tag []
                        Right (V.TaggedUnionDecl taggedUnionType)


    PaFuncDecl funcSignature funcBody -> do
            -- 5. populate symbol table with function parameters
            env2 <- 
                foldM 
                    (\acc (id, typeAnnot) ->
                        if member (snd id) acc then
                            Left (KErrorDuplicatedId [id])
                        else
                            Right (acc |> (snd id, KeliSymLocalConst id (V.getTypeRef typeAnnot))))
                env
                (V.funcDeclParams funcSignature)
            
            -- 6. type check the function body
            (_, typeCheckedBody) <- verifyExpr (Context 0 env2 importedEnvs) CanBeAnything funcBody
            let bodyType = getType typeCheckedBody

            let verifiedReturnType = V.funcDeclReturnType funcSignature
            -- 7. ensure body type adheres to return type
            case verifiedReturnType of
                -- if return type is not declared, the return type of this function is inferred as the type of the body
                V.TypeUndefined ->
                    Right (V.FuncDecl (funcSignature {V.funcDeclReturnType = bodyType}) typeCheckedBody)

                _ -> do
                    case unify typeCheckedBody verifiedReturnType of
                        Left err ->
                            Left err

                        Right _ ->
                        -- if body type match expected return types
                            Right (V.FuncDecl funcSignature typeCheckedBody)
    
    PaIdlessDecl pos expr -> do
        (_, result) <- typeCheckExpr (Context 0 env importedEnvs) CanBeAnything expr
        case result of
            First checkedExpr -> do
                -- search for the `toString` function that match the type of this expr
                lookupResult <- lookupEnvs [] ("toString") env importedEnvs
                case lookupResult of
                    Just (scope, KeliSymFunc candidateFuncs) -> do
                        case find 
                                (\f -> 
                                    let (_,typeAnnot) =  V.funcDeclParams f !! 0 in
                                    case unify checkedExpr (V.getTypeRef typeAnnot) of 
                                        Right{} -> True
                                        Left{}  -> False) candidateFuncs of
                            Just f ->
                                let finalExpr = 
                                        V.Expr 
                                            (V.FuncCall {
                                                V.funcCallParams = [checkedExpr],
                                                V.funcCallIds    = [(pos,"toString")],
                                                V.funcCallRef    = (scope, f)
                                            }) 
                                            V.TypeString
                                in Right (V.IdlessDecl finalExpr)

                            -- if no `toString` function is defined for the particular type
                            Nothing ->
                                Right (V.IdlessDecl checkedExpr)

                    -- if no `toString` function is defined for the particular type
                    _ ->
                        Right (V.IdlessDecl checkedExpr)

            Second typeAnnot -> 
                Left (KErrorCannotDeclareTypeAsAnonymousConstant typeAnnot)
            
            Third tags ->
                Left (KErrorCannotDeclareTagAsAnonymousConstant tags)
        
    PaGenericTypeDecl typeConstructorName ids typeParams typeBody -> do
        -- 1. verify all type params
        verifiedTypeParams <- mapM (verifyBoundedTypeVar (Context 0 env importedEnvs)) typeParams 

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
                    KeliSymTaggedUnion (V.TaggedUnion typeConstructorName ids [] verifiedTypeParams')))

        -- 4. type check the body
        (_, typeCheckedBody) <- (typeCheckExpr (Context 0 env3 importedEnvs) StrictlyAnalyzingType typeBody) 
        case typeCheckedBody of
            Third tag -> do
                taggedUnion <- linkTagsTogether typeConstructorName ids tag verifiedTypeParams'
                Right (V.TaggedUnionDecl taggedUnion)

            _ ->
                undefined


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
                                        (key, substituteSelfType (V.TypeTaggedUnion tagUnionType) (V.getTypeRef typeAnnot))) 
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
    
        V.TypeObject name propTypePairs ->
            let updatedPropTypePairs = 
                    map 
                        (\(prop, type') -> 
                            (prop, substituteSelfType source type'))
                        propTypePairs
            in  (V.TypeObject name updatedPropTypePairs)

        V.TypeTaggedUnion (V.TaggedUnion (_,name1) _ _ _) ->
            -- TODO: something fishy here, not sure if got bug or not
            case source of
                V.TypeTaggedUnion (V.TaggedUnion (_,name2) _ _ _) ->
                    if name1 == name2 then
                        source
                    else 
                        target

                _ ->
                    target

        _ ->
            target


toSymbol ::  V.Decl -> Maybe KeliSymbol
toSymbol decl = 
    case decl of
        V.ConstDecl id expr ->
            Just (KeliSymGlobalConst id (getType expr))

        V.FuncDecl signature _ ->
            Just (KeliSymFunc [signature])

        V.IdlessDecl _ ->
            Nothing

        V.ObjectAliasDecl id expectedPropTypePairs ->
            Just (KeliSymType (V.TypeObject (Just id) expectedPropTypePairs))

        V.TaggedUnionDecl t ->
            Just (KeliSymTaggedUnion t)
