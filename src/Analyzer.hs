{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Analyzer where

import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered ((|>), assocs, member) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError
import Env
import TypeCheck
import Util
import Unify

analyze :: [Raw.Decl] -> Either [KeliError] [V.Decl]
analyze decls = 
    let (errors, _, analyzedDecls) = analyzeDecls initialEnv decls in
    if length errors > 0 then
        Left errors
    else
        -- sorting is necessary, so that the transpilation order will be correct
        -- Smaller number means will be transpiled first
        Right (sortOn (
                \x -> case x of 
                    V.TaggedUnionDecl{}    -> 1
                    V.RecordAliasDecl{}    -> 2
                    V.FuncDecl{}           -> 3
                    V.ConstDecl{}          -> 4
                    V.IdlessDecl{}         -> 5
                ) (analyzedDecls)) 

extractSymbols :: Env -> [KeliSymbol]
extractSymbols env = map snd (assocs env)

data TypeDecl = 
    TypeDecl 
        [V.StringToken] -- type signature
        V.Type          -- type body

analyzeDecls 
    :: Env -- previous env
    -> [Raw.Decl] -- parsed input
    -> ([KeliError], Env, [V.Decl]) -- (accumulatedErrors, newEnv, symbols)

analyzeDecls env rawDecls = 
    let (errors, updatedEnv, _, analyzedDecls) = analyzeDecls' env rawDecls [] in
    (errors, updatedEnv, analyzedDecls)

-- this function will perform multipassing
-- so that declaration order will be insignificant
analyzeDecls' 
    :: Env 
    -> [Raw.Decl] 
    -> [V.Decl]       -- previous verified decls
    -> 
        ([KeliError], -- errors
        Env,          -- updated env
        [Raw.Decl],   -- declarations that failed to pass the type checker
        [V.Decl])     -- declarations that passed the type checker

analyzeDecls' env inputRawDecls prevVerifiedDecls =
    let (currentErrors, updatedEnv, failedDecls, currentVerifiedDecls) =
            foldl'
                ((\(errors, prevEnv, prevFailedDecls, prevPassedDecls) currentRawDecl -> 
                    let (newErrors, newEnv, newFailedDecls, newPassedDecls) = 
                            -- Do partial analyzation (to get function signature (if it's a function))
                            case analyzeDecl currentRawDecl prevEnv of
                                Right partiallyAnalyzedDecl -> 
                                    let updatedPrevEnv = 
                                            case partiallyAnalyzedDecl of
                                                PaFuncDecl f _ ->
                                                    insertSymbolIntoEnv (KeliSymFunc [f]) prevEnv

                                                _ ->
                                                    Right prevEnv in

                                    case updatedPrevEnv of
                                        Right updatedPrevEnv' ->
                                            case analyzePaDecl partiallyAnalyzedDecl updatedPrevEnv' of
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
        analyzeDecls' updatedEnv failedDecls (prevVerifiedDecls ++ currentVerifiedDecls)

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
        Raw.Expr

    | PaGenericTypeDecl 
        Raw.StringToken     -- name
        [Raw.StringToken]   -- trailing ids
        [(Raw.StringToken, Raw.Expr)] -- type params
        Raw.Expr            -- type body

analyzeDecl :: Raw.Decl -> Env -> Either KeliError PaDecl
analyzeDecl rawDecl env = case rawDecl of
    Raw.ConstDecl c -> 
        Right (PaConstDecl c)
    
    Raw.IdlessDecl expr ->
        Right (PaIdlessDecl expr)

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
            let ctx = Context 0 env

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
                            (_, verifiedTypeAnnotation) <- verifyTypeAnnotation (Context 0 env1) typeAnnot
                            return (id, verifiedTypeAnnotation)) 
                        funcParams

            -- 3. verify return type
            verifiedReturnType <- 
                (case returnType of 
                    Just t -> do
                        (_, verifiedTypeAnnot) <- verifyTypeAnnotation (Context 0 env1) t
                        Right (V.getTypeRef verifiedTypeAnnot)

                    Nothing ->
                        Right (V.TypeUndefined))



            -- 4. Return this function signature
            let funcSig = V.FuncSignature{
                        V.funcDeclDocString = Nothing,
                        V.funcDeclIds = funcIds,
                        V.funcDeclGenericParams = verifiedGenericParams',
                        V.funcDeclParams = verifiedFuncParams,
                        V.funcDeclReturnType = verifiedReturnType
                    }

            Right (PaFuncDecl funcSig funcBody)

    
analyzePaDecl :: PaDecl -> Env -> Either KeliError V.Decl
analyzePaDecl paDecl env = case paDecl of
    PaConstDecl Raw.Const {
        Raw.constDeclId=id,
        Raw.constDeclValue=expr
    } -> 
        case expr of
            Raw.Id _ -> 
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
                let updatedEnv = env |> (snd id, KeliSymType V.TypeSelf)
                (_, result) <- typeCheckExpr (Context 0 updatedEnv) CanBeAnything expr
                case result of
                    First typeCheckedExpr ->
                        Right (V.ConstDecl id typeCheckedExpr)

                    Second (V.TypeAnnotCompound _ _ t) -> do
                        case t of
                            V.TypeRecord _ expectedPropTypePairs ->
                                Right (V.RecordAliasDecl id expectedPropTypePairs)

                            V.TypeTaggedUnion t ->
                                Right (V.TaggedUnionDecl t)
                            
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
                            Right (acc |> (snd id, KeliSymConst id (V.getTypeRef typeAnnot))))
                env
                (V.funcDeclParams funcSignature)
            
            -- 6. type check the function body
            (_, typeCheckedBody) <- verifyExpr (Context 0 env2) CanBeAnything funcBody
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
    
    PaIdlessDecl expr -> do
        (_, result) <- typeCheckExpr (Context 0 env) CanBeAnything expr
        case result of
            First checkedExpr ->
                Right (V.IdlessDecl checkedExpr)

            Second typeAnnot -> 
                Left (KErrorCannotDeclareTypeAsAnonymousConstant typeAnnot)
            
            Third tags ->
                Left (KErrorCannotDeclareTagAsAnonymousConstant tags)
        
    PaGenericTypeDecl typeConstructorName ids typeParams typeBody -> do
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
                    KeliSymTaggedUnion (V.TaggedUnion typeConstructorName ids [] verifiedTypeParams')))

        -- 4. type check the body
        (_, typeCheckedBody) <- (typeCheckExpr (Context 0 env3) StrictlyAnalyzingType typeBody) 
        case typeCheckedBody of
            Third tag -> do
                taggedUnion <- linkTagsTogether typeConstructorName ids tag verifiedTypeParams'
                Right (V.TaggedUnionDecl taggedUnion)

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


toSymbol ::  V.Decl -> Maybe KeliSymbol
toSymbol decl = 
    case decl of
        V.ConstDecl id expr ->
            Just (KeliSymConst id (getType expr))

        V.FuncDecl signature _ ->
            Just (KeliSymFunc [signature])

        V.IdlessDecl _ ->
            Nothing

        V.RecordAliasDecl id expectedPropTypePairs ->
            Just (KeliSymType (V.TypeRecord (Just id) expectedPropTypePairs))

        V.TaggedUnionDecl t ->
            Just (KeliSymTaggedUnion t)
