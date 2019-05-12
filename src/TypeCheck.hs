{-# LANGUAGE BangPatterns #-}
module TypeCheck where

import Control.Monad
import Data.Either
import Data.Maybe
import qualified Data.List as List
import Module
import Data.Map.Ordered ((|>), lookup, member) 
import qualified Data.Map.Strict as Map
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup, id, head)

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError
import Env
import Util (
    OneOf3(First, Second, Third), 
    MatchResult(GotDuplicates,ZeroIntersection,GotExcessive,Missing,PerfectMatch),
    match,
    findDuplicates)

data Assumption 
    = StrictlyAnalyzingType
    | CanBeAnything 
    deriving (Show)

typeCheckExpr 
    :: Context 
    -> Assumption 
    -> Raw.Expr 
    -> Either KeliError (Context, OneOf3 V.Expr V.TypeAnnotation ([V.UnlinkedTag]))
typeCheckExpr ctx@(Context _ env importedEnvs) assumption expression = case expression of 
    Raw.TaggedUnion casesKeyword funcCallTail -> do
        unlinkedTags <- forM 
            funcCallTail 
            (\(funcIds, funcCallParams) -> do
                case (funcIds, funcCallParams) of
                    -- if is carryless tag
                    (tagname:[], []) -> 
                        Right (V.UnlinkedCarrylessTag tagname)

                    -- is carryful tag (because have carry)
                    (tagname:[], expectedCarryType:[]) -> do
                        (_,verifiedCarryType) <- verifyTypeAnnotation ctx expectedCarryType
                        Right (V.UnlinkedCarryfulTag tagname verifiedCarryType)

                    -- or else
                    _ ->
                        Left (KErrorIncorrectTagDeclSyntax funcIds)) 

        Right (ctx, Third unlinkedTags)

    Raw.IncompleteFuncCall expr positionOfTheDotOperator -> do
        case expr of
            Raw.Lambda param@(_,paramName) body isShortHand ->
                if isShortHand then
                    typeCheckExpr ctx assumption 
                        (Raw.Lambda param (Raw.IncompleteFuncCall body positionOfTheDotOperator) isShortHand)
                
                else
                    defaultResult
            
            _ ->
                defaultResult

        where 
            defaultResult = do
                (_,typeCheckedExpr) <- typeCheckExpr ctx assumption expr
                Left (KErrorIncompleteFuncCall typeCheckedExpr positionOfTheDotOperator)

    Raw.Array exprs pos ->
        case exprs of
            [] ->
                let boundedTypeVar = V.BoundedTypeVar (V.newStringToken "A") Nothing in
                let arrayType = V.TypeTaggedUnion (newArrayType boundedTypeVar) in
                let (ctx3, subst) = instantiateTypeVar ctx [boundedTypeVar] in
                let arrayType' = applySubstitutionToType subst arrayType in
                Right (ctx3, First (V.Expr
                    (V.Array [] pos)
                    (arrayType')))

            x:xs -> do
                (ctx2, elements) <- verifyExprs ctx assumption (x:xs)
                case elements of
                    [] ->
                        error "impossible"
                    
                    firstElement:tailElements -> do
                        let typeOfFirstElem = getType firstElement
                        -- make sure every element have the same type as the first element 
                        !_ <- forM tailElements (\element -> unify ctx2 emptySubstitution element typeOfFirstElem)
                        Right (ctx2, First (V.Expr
                            (V.Array elements pos)
                            (V.TypeTaggedUnion (newArrayType typeOfFirstElem))))

    Raw.NumberExpr(pos,n) -> 
        case n of 
            Left intValue ->
                Right (ctx, First (V.Expr (V.IntExpr (pos, intValue)) ( V.TypeInt)))

            Right doubleValue ->
                Right (ctx, First (V.Expr (V.DoubleExpr (pos, doubleValue)) ( V.TypeFloat)))

    Raw.StringExpr (pos, str) -> 
        Right (ctx, First (V.Expr (V.StringExpr (pos, str)) ( V.TypeString)))

    Raw.Id token@(_,id) -> 
        let relatedSymbols = lookupEnvs id env importedEnvs in
        case relatedSymbols of 
            [] ->
                Left (KErrorUsingUndefinedId token)

            (scope, KeliSymGlobalConst ref type'):[] -> 
                Right (ctx, First (V.Expr (V.GlobalId token ref scope) type'))

            (_, KeliSymLocalConst ref type'):[] -> 
                Right (ctx, First (V.Expr (V.LocalId token ref) type'))
        
            (scope, KeliSymType t@(V.TypeObject name propTypePairs)):[] -> 
                -- Question: How are we gonna know if user is using this as type annotation or as object constructor?
                -- Answer: Using assumptions
                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (ctx, Second (V.TypeAnnotSimple token t)))

                            
                    CanBeAnything ->
                        (Right (ctx, First (V.Expr (V.ObjectConstructor name propTypePairs) ((V.TypeObjectConstructor name propTypePairs)))))

            (scope, KeliSymType t@((V.TypeTaggedUnion (V.TaggedUnion name _ tags innerTypes)))):[] ->
                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (ctx, Second (V.TypeAnnotSimple token t)))

                    CanBeAnything ->
                        (Right (ctx, First 
                            (V.Expr (V.TagConstructorPrefix name) 
                            (V.TypeTagConstructorPrefix name tags innerTypes scope))))
            
            (scope, KeliSymType t):[] ->
                (Right (ctx, Second (V.TypeAnnotSimple token t)))
            
            (scope, KeliSymTaggedUnion t@(V.TaggedUnion _ _ tags typeParams)):[] ->
                let name = token in
                case assumption of
                    StrictlyAnalyzingType ->
                        if length typeParams > 0 then
                            Right (ctx, First (V.Expr (V.TypeConstructorPrefix name) ((V.TypeTypeConstructor t))))
                        else
                            Right (ctx, Second (V.TypeAnnotSimple name (V.TypeTaggedUnion t)))

                    CanBeAnything -> 
                        (Right (ctx, First 
                            (V.Expr (V.TagConstructorPrefix name) 
                            (V.TypeTagConstructorPrefix name tags typeParams scope))))

            -- if more than one symbols
            symbols -> 
                Left (KErrorAmbiguousUsage [token] symbols)

    -- (1) check if user wants to create an object value
    Raw.FuncCall ((Raw.Id (_,"$")):(firstValue:tailValues)) keys -> do
        -- NOTE: 
        --  Because of the following line of code,
        --  the following recursive object type cannot be declared:
        --
        --      fruit = object.next fruit;
        -- 
        -- It's ok, because we shouldn't allow user to create recursive objects (which will form infinite type)
        (ctx2, firstValue') <- typeCheckExpr ctx assumption firstValue  -- <-- this line
        case firstValue' of 
            -- (1.1) assume user want to create a object value
            First _ -> do
                (ctx3, keyValuePairs) <- verifyKeyValuePairs ctx2 keys (firstValue:tailValues)
                Right 
                    (ctx3, First
                        (V.Expr
                            (V.Object keyValuePairs) 
                            ( (V.TypeObject Nothing 
                                (List.map (\(k, expr) -> (k, getType expr)) keyValuePairs)))))
            
            -- (1.2) assume user want to declare a object type
            Second _ -> do
                (ctx3, keyTypePairs) <- verifyKeyTypeAnnotPairs ctx keys (firstValue:tailValues)
                Right (ctx3, Second (V.TypeAnnotObject keyTypePairs))

            Third tag -> 
                Left (KErrorExpectedExprOrTypeButGotTag tag)
                
    -- (2) check if user is using javascript ffi
    Raw.FuncCall 
        ((Raw.Id (_,"ffi")):((Raw.StringExpr jsCode):[])) 
        ((_,"javascript"):[]) ->
        Right (ctx, First (V.Expr (V.FFIJavascript jsCode) ( V.TypeUndefined)))

    -- (3) check if user is invoking magic functions
    Raw.FuncCall (firstParam:tailParams) funcIds -> do
        (ctx2, typeCheckedFirstParam) <- verifyExpr ctx assumption firstParam
        continuePreprocessFuncCall1 ctx assumption typeCheckedFirstParam tailParams funcIds


    Raw.Lambda param body _ ->
        let (ctx2, inputTVar) = createNewTVar ctx Nothing in
        let (ctx3, outputTVar) = createNewTVar ctx2 Nothing in
        Right (ctx3, First (V.Expr 
            (V.PartiallyInferredLambda param body)
            (V.TypeTaggedUnion (newFunctionType inputTVar outputTVar))))

continuePreprocessFuncCall1 
    :: Context 
    -> Assumption 
    -> V.Expr 
    -> [Raw.Expr] 
    -> [V.StringToken]
    -> Either KeliError (Context, OneOf3 V.Expr V.TypeAnnotation ([V.UnlinkedTag]))

-- (A) check if user is invoking object constructor
continuePreprocessFuncCall1 ctx assumption 
    (V.Expr _ (V.TypeObjectConstructor aliasedName expectedPropTypePairs))
    (firstValue:tailValues) 
    (properties) = do
        (ctx3, values)  <- verifyExprs ctx assumption (firstValue:tailValues)
        let actualPropValuePairs = zip properties values
        let actualObject = V.Expr 
                (V.Object actualPropValuePairs) 
                (V.TypeObject aliasedName (zip properties (map getType values)))
        (substitution, _) <- unify ctx3 emptySubstitution actualObject (V.TypeObject aliasedName expectedPropTypePairs)
        let resultingType = applySubstitutionToType substitution (V.TypeObject aliasedName expectedPropTypePairs)
        Right (ctx3, First (V.Expr (V.Object actualPropValuePairs) resultingType))

-- (B) check if user is performing function application
continuePreprocessFuncCall1 ctx assumption 
    func@(V.Expr _ (V.TypeTaggedUnion (V.TaggedUnion (_,"Function") _ _ (expectedInputType:expectedOutputType:[]))))
    (appliedValue:[]) 
    ((_,"apply"):[]) = do
        (ctx3, value) <- verifyExpr ctx assumption appliedValue
        -- check if value match with expectedInputType
        !_ <- unify ctx emptySubstitution value expectedInputType
        Right (ctx3, First(V.Expr (V.FuncApp func value) expectedOutputType))

-- (C) check if user is calling tag matchers
continuePreprocessFuncCall1 ctx assumption 
    subject@(V.Expr _ (V.TypeTaggedUnion (V.TaggedUnion _ _ expectedTags innerTypes)))
    (headBranch:tailBranches) 
    funcIds@((_,"if"):_) = do
        -- 0. take out else branch
        let (rawElseBranches, noElseBranches) = 
                List.partition 
                    (\((_,x),_) -> x == "else") 
                    (List.zip funcIds (headBranch:tailBranches))

        let (elselessFuncIds, elselessParams) = unzip noElseBranches

        -- 1. check if syntax is correct
        !_ <- mapM (\(pos,x) -> if x == "if" || x == "else" then Right() else Left (KErrorExpectedKeywordIfOrDefault (pos,x))) (evens elselessFuncIds)
        !_ <- mapM (\(pos,x) -> if x == ":"  then Right() else Left (KErrorExpectedColon (pos,x))) (odds elselessFuncIds)

        let tags = evens elselessParams
        let branches = odds elselessParams
        let rawTagBranches = zip tags branches

        -- 2.1 type check raw tag branches
        typeCheckedTagBranches <- 
                mapM 
                    (\nextRawTagBranch -> do
                        (_, typeCheckedTagBranch) <- typeCheckTagBranch ctx expectedTags nextRawTagBranch
                        Right typeCheckedTagBranch)
                    rawTagBranches

        -- 2.2 type check raw else branches
        (ctx3, typeCheckedElseBranches) <- verifyExprs ctx assumption (map snd rawElseBranches)

        -- 3. check if each branch have the same type with the first branch
        case allBranchTypeAreSame ctx3 (typeCheckedTagBranches ++ map (V.ElseBranch) typeCheckedElseBranches) of
            Left err ->
                case err of
                    KErrorTypeMismatch actualExpr actualType expectedType ->
                        case typeCheckedTagBranches of 
                            [] ->
                                undefined

                            x:_ ->
                                Left (KErrorNotAllBranchHaveTheSameType actualExpr actualType expectedType x)

                    _ ->
                        Left err

            Right branchType ->
                -- 4. check for exhaustiveness
                let actualTagnames = concatMap (\b -> case b of 
                        V.CarrylessTagBranch (V.VerifiedTagname name) _ ->
                            [name]
                        V.CarryfulTagBranch (V.VerifiedTagname name) _ _ ->
                            [name]
                        V.ElseBranch{} -> 
                            []
                        ) typeCheckedTagBranches in
                
                let expectedTagnames = map (V.tagnameOf) expectedTags in

                case match actualTagnames expectedTagnames of
                    PerfectMatch ->
                        Right (ctx3, First (V.Expr 
                            (V.TagMatcher subject typeCheckedTagBranches Nothing) 
                            branchType))

                    GotDuplicates duplicates ->
                        Left (KErrorDuplicatedTags duplicates)

                    Missing missingTagnames ->
                        case typeCheckedElseBranches of
                            [] ->
                                let missingTags = 
                                        filter 
                                            (\t -> snd (V.tagnameOf t) `elem` missingTagnames)
                                            expectedTags

                                in Left (KErrorMissingTags subject missingTags)

                            elseBranch:[] ->
                                Right (ctx3, First (V.Expr 
                                    (V.TagMatcher subject typeCheckedTagBranches (Just elseBranch)) 
                                    branchType))

                            _ ->
                                Left (KErrorMoreThanOneElseBranch (map fst rawElseBranches))

                    other ->
                        error (show other)
                        -- error "Shoudln't reach here, because `typeCheckTagBranch` already check for unknown tags"


-- (D) check if user is calling object getter/setter
continuePreprocessFuncCall1 ctx assumption 
    subject@(V.Expr _ objectType@(V.TypeObject _ kvs))
    (tailParams) 
    funcIds@(propertyName:[]) = do
        case List.find (\((_,key),_) -> key == snd propertyName) kvs of
            Just (_, expectedType) -> 
                -- Check if is getter or setter
                case tailParams of
                    [] -> -- is getter
                        Right (ctx, First (V.Expr (V.ObjectGetter subject propertyName) expectedType))
                    
                    secondParam:[] -> -- is setter
                        case secondParam of
                            -- if is lambda setter
                            -- TODO: need to check if expectedType is Function
                            -- something squishy is happening
                            Raw.Lambda lambdaParam lambdaBody _ -> do
                                updatedEnv <- insertSymbolIntoEnv (KeliSymLocalConst lambdaParam expectedType) (contextEnv ctx) 
                                (ctx4, verifiedLambdaBody) <- verifyExpr (ctx{contextEnv = updatedEnv}) assumption lambdaBody

                                (subst2, verifiedLambdaBody') <- unify ctx4 emptySubstitution verifiedLambdaBody expectedType
                                let returnType = applySubstitutionToType subst2 objectType
                                Right (ctx4, First (V.Expr 
                                    (V.ObjectLambdaSetter 
                                        subject
                                        propertyName
                                        lambdaParam
                                        verifiedLambdaBody')
                                    (returnType)))

                            -- else is value setter
                            _ -> do
                                (ctx3, newValue) <- verifyExpr ctx assumption secondParam
                                (substitution, newValue') <- unify ctx3 emptySubstitution newValue expectedType
                                let returnType = applySubstitutionToType substitution objectType
                                Right (ctx3, First (V.Expr 
                                    (V.ObjectSetter subject propertyName newValue') 
                                    (returnType)))
                    
                    -- else
                    _ ->
                        continuePreprocessFuncCall2 ctx assumption subject tailParams funcIds

            Nothing -> 
                continuePreprocessFuncCall2 ctx assumption subject tailParams funcIds

-- (E) check if user is invoking tag constructor prefix
continuePreprocessFuncCall1 ctx assumption 
    subject@(V.Expr _ (V.TypeTagConstructorPrefix taggedUnionName tags _ scope))
    (tailParams) 
    (tagname:[]) = do
        case List.find (\t -> snd (V.tagnameOf t) == snd tagname) tags of
            -- if is carryless tag
            Just (V.CarrylessTag tag (V.TaggedUnion name ids _ innerTypes)) -> 
                let (ctx3, subst) = instantiateTypeVar ctx innerTypes in
                let resultingInnerTypes = map (applySubstitutionToType subst) innerTypes in
                let belongingUnion = V.TaggedUnion name ids tags resultingInnerTypes in
                (Right (ctx3, First (V.Expr 
                    (V.CarrylessTagExpr taggedUnionName tag tagname scope) 
                    ((V.TypeTaggedUnion belongingUnion)))))

            -- if is carryful tag
            Just (V.CarryfulTag _ expectedCarryType taggedUnion@(V.TaggedUnion name ids _ innerTypes)) ->
                case tailParams of
                    carryExpr:[] -> do
                        -- instantiate type vars
                        let (ctx3, subst1) = instantiateTypeVar ctx innerTypes 
                        let expectedCarryType' = applySubstitutionToType subst1 expectedCarryType

                        -- verify carry expr
                        (ctx4, verifiedCarryExpr) <- verifyExpr ctx3 assumption carryExpr
                        (subst2, verifiedCarryExpr') <- unify ctx4 emptySubstitution verifiedCarryExpr expectedCarryType'

                        let resultingTaggedUnionType = 
                                applySubstitutionToType subst2 
                                    (applySubstitutionToType subst1 
                                        (V.TypeTaggedUnion taggedUnion))

                        -- update expected carry type of every carryful tag

                        Right (ctx4, First (V.Expr 
                            (V.CarryfulTagExpr taggedUnionName tagname verifiedCarryExpr' scope)
                            (resultingTaggedUnionType)))

                    _ ->
                        Left (KErrorIncorrectUsageOfTagConstructorPrefix subject)

            Nothing ->
                Left (KErrorTagNotFound tagname taggedUnionName tags)


-- (F) check if user is invoking type constructor
continuePreprocessFuncCall1 ctx assumption 
    (V.Expr subject (V.TypeTypeConstructor (V.TaggedUnion name ids tags _)))
    (tailParams) 
    (funcIds) = do
        if map snd funcIds /= map snd ids then
            Left (KErrorTypeConstructorIdsMismatch ids funcIds)
        else do
            -- TODO: check if type param conforms to type constraint
            (ctx3, types) <- verifyTypeAnnotations ctx tailParams
            Right (ctx3, Second 
                (V.TypeAnnotCompound
                    name
                    (zip ids types)
                    (V.TypeTaggedUnion (V.TaggedUnion name ids tags (map V.getTypeRef types)))))

-- otherwise
continuePreprocessFuncCall1 ctx assumption firstParam tailParams funcIds =
    continuePreprocessFuncCall2 ctx assumption firstParam tailParams funcIds

continuePreprocessFuncCall2 
    :: Context 
    -> Assumption 
    -> V.Expr 
    -> [Raw.Expr] 
    -> [V.StringToken]
    -> Either KeliError (Context, OneOf3 V.Expr V.TypeAnnotation ([V.UnlinkedTag]))
continuePreprocessFuncCall2 ctx assumption firstParam tailParams funcIds = 
    case funcIds of
        -- check if user is calling type casting
        (_, "as"):[] -> 
            case tailParams of
                castType:[] -> do
                    (ctx3, castType') <- typeCheckExpr ctx StrictlyAnalyzingType castType
                    castType'' <- extractType castType'
                    case firstParam of
                        (V.Expr expr' (V.TypeUndefined)) ->
                            Right (ctx3, First (V.Expr expr' (V.getTypeRef castType'')))

                        _ ->
                            -- TODO: check for other kind of type casting
                            undefined

                _ ->
                    treatAsNormalFuncCall

        -- otherwise
        _ -> 
            treatAsNormalFuncCall

    where
        treatAsNormalFuncCall = do
            (ctx2, verifiedTailParams) <- verifyExprs ctx assumption tailParams
            (ctx3, result) <- lookupFunction ctx2 assumption (firstParam:verifiedTailParams) funcIds
            Right (ctx3, First result)

verifyTypeAnnotation :: Context -> Raw.Expr -> Either KeliError (Context, V.TypeAnnotation) 
verifyTypeAnnotation ctx rawExpr = do
    (ctx2, expr) <- typeCheckExpr ctx StrictlyAnalyzingType rawExpr
    type' <- extractType expr 
    Right (ctx2, type')

verifyTypeAnnotations 
    :: Context 
    -> [Raw.Expr]
    -> Either KeliError (Context, [V.TypeAnnotation]) 
verifyTypeAnnotations ctx rawExprs = do
    foldM 
        (\(prevCtx, verifiedTypes) nextRawExpr -> do
            (nextCtx, verifiedType) <- verifyTypeAnnotation prevCtx nextRawExpr
            Right (nextCtx, verifiedTypes ++ [verifiedType]))
        (ctx, [])
        rawExprs

verifyExpr :: Context -> Assumption -> Raw.Expr -> Either KeliError (Context, V.Expr) 
verifyExpr ctx assumption rawExpr = do
    (ctx2, expr) <- typeCheckExpr ctx assumption rawExpr
    expr' <- extractExpr expr 
    Right (ctx2, expr')

verifyExprs 
    :: Context 
    -> Assumption 
    -> [Raw.Expr]
    -> Either KeliError (Context, [V.Expr]) 
verifyExprs ctx assumption rawExprs =
    foldM 
        (\(prevCtx, verifiedExprs) nextRawExpr -> do
            (nextCtx, verifiedExpr) <- verifyExpr prevCtx assumption nextRawExpr
            Right (nextCtx, verifiedExprs ++ [verifiedExpr]))
        (ctx, [])
        rawExprs

data MatchFuncResult 
    = PerfectlyMatchedFuncFound 
        Context         -- updated context
        V.Expr          -- resulting func call expr
        V.FuncSignature -- matching funcion

    | PartiallyMatchedFuncFound -- means the 2nd params onward does not match expected types
        KeliError  -- corresponding error (should be type mismatch error)

    | StillNoMatchingFunc
    deriving(Show)

-- NOTE: params should be type checked using typeCheckExprs before passing into the lookupFunction function
lookupFunction 
    :: Context 
    -> Assumption 
    -> [V.Expr]
    -> [Raw.StringToken]
    -> Either KeliError (Context, V.Expr)

lookupFunction ctx@(Context _ env importedEnvs) assumption funcCallParams funcIds = do
    -- lookup the function being invoked from all the imported modules
    let lookupResult = lookupEnvs (List.intercalate "$" (map snd funcIds)) env importedEnvs
    let results = map (lookupFunction' ctx funcCallParams funcIds) lookupResult
    let matchingFuncs = rights results
    case matchingFuncs of
        -- if no matching function is found
        [] ->
            let errors = lefts results in
            case errors of
                e:[] ->
                    case e of 
                        KErrorPartiallyMatchedFuncFound e' ->
                            Left e'
                        _ ->
                            Left e

                _ ->
                    -- the following code is to improve error reporting
                    case List.find (\e -> case e of KErrorPartiallyMatchedFuncFound{}->True; _->False) errors of
                        Just e ->
                            Left e

                        _ ->
                            Left (KErrorUsingUndefinedFunc funcIds [])


        -- if only ONE matching function is found
        (updatedCtx,funcCallExpr,_):[] ->
            Right (updatedCtx, funcCallExpr)

        -- if more than one matching functions are found
        _ ->
            -- remove super generic function (i.e. where the first parameter can be any type)
            -- this is to allow function specialization
            let isSuperGeneric f = 
                    case V.funcDeclParams f of
                        (_, V.TypeAnnotSimple _ (V.BoundedTypeVar _ _)):_ ->
                            True

                        _ -> 
                            False
            in
            case filter (\(_,_,f) -> not (isSuperGeneric f)) matchingFuncs of
                -- if only one function left after removing 
                (updatedCtx, funcCallExpr,_ ):[] ->
                    Right (updatedCtx, funcCallExpr)
                
                -- if no function left or more than one functions left
                _ ->
                    Left (KErrorAmbiguousUsage funcIds lookupResult)




-- this is a helper function to lookup for matching function within a SINGLE module only 
lookupFunction'
    :: Context 
    -> [V.Expr]
    -> [Raw.StringToken]
    -> (V.Scope, KeliSymbol)
    -> Either KeliError (Context, V.Expr, V.FuncSignature)

lookupFunction' ctx (firstParam:tailParams) funcIds lookupResult = 
    case lookupResult of
        (scope, KeliSymFunc candidateFuncs) -> do
            case (foldl 
                    (\result currentFunc ->
                        case result of
                        p@PerfectlyMatchedFuncFound{} -> p

                        p@PartiallyMatchedFuncFound{} -> p
                        
                        -- if
                        StillNoMatchingFunc -> -- then continue the loop for searching matching functions
                            if length (firstParam:tailParams) /= length (V.funcDeclParams currentFunc) then
                                StillNoMatchingFunc
                            else do
                                -- (A) instantiate type variables  
                                let (ctx2, subst1) = instantiateTypeVar ctx (V.funcDeclGenericParams currentFunc)

                                -- (B) apply substitution to every expected func param types
                                let expectedParamTypes@(firstExpectedParamType:_) = 
                                        map (\(_,paramTypeAnnot) -> 
                                            applySubstitutionToType subst1 (V.getTypeRef paramTypeAnnot)) (V.funcDeclParams currentFunc) 

                                -- (C) check if the firstParam matches the firstParamExpectedType
                                case unify ctx2 subst1 firstParam firstExpectedParamType of
                                    -- if matches, continue to unify the following params
                                    Right (subst2,firstParam') -> 
                                        case unifyMany ctx2 subst2 (firstParam':tailParams) expectedParamTypes of
                                            Left err ->
                                                PartiallyMatchedFuncFound err

                                            Right (subst3, inferredFuncCallParams) -> 
                                                let subst4 = composeSubst subst2 subst3 in
                                                let expectedReturnType = applySubstitutionToType subst1 (V.funcDeclReturnType currentFunc) in
                                                let expectedReturnType' = applySubstitutionToType subst4 expectedReturnType in
                                                let funcCall = V.Expr (V.FuncCall inferredFuncCallParams funcIds (scope,currentFunc)) (expectedReturnType') in
                                                PerfectlyMatchedFuncFound ctx2 funcCall currentFunc
                                                    
                                    -- if not, 
                                    Left _ ->
                                        StillNoMatchingFunc)

                    
                        StillNoMatchingFunc

                        -- This sorting is necessary so that the compiler will look for more specialized (a.k.a less generic) function first
                        (List.sortOn (\f -> length (V.funcDeclGenericParams f)) candidateFuncs)) of

                PerfectlyMatchedFuncFound newCtx funcCallExpr matchingFunc ->
                    Right (newCtx, funcCallExpr, matchingFunc)

                PartiallyMatchedFuncFound err ->
                    Left (KErrorPartiallyMatchedFuncFound err)

                StillNoMatchingFunc ->
                    Left (KErrorUsingUndefinedFunc funcIds candidateFuncs)
        
        _ -> 
            Left (KErrorUsingUndefinedFunc funcIds [])


lookupFunction' ctx@(Context _ env importedEnvs)  [] funcIds lookupResult = error "impossible"

verifyBoundedTypeVar 
    :: Context 
    -> (Raw.StringToken, Raw.Expr) 
    -> Either KeliError (V.StringToken, Maybe V.TypeConstraint)
verifyBoundedTypeVar ctx (name, expr) = do
    (_, result) <- verifyTypeAnnotation ctx expr
    case result of
        V.TypeAnnotSimple _ (V.TypeType) ->
            Right (name, Nothing)

        _ ->
            Left (KErrorInvalidBoundedTypeVarDecl expr)


hardConformsTo :: V.Type -> (Maybe V.TypeConstraint) -> Bool
_ `hardConformsTo` Nothing = True
_ `hardConformsTo` (Just constraint) = 
    case constraint of
        V.ConstraintAny -> 
            True

extractTag :: OneOf3 V.Expr V.TypeAnnotation [V.UnlinkedTag] -> Either KeliError [V.UnlinkedTag]
extractTag x =
    case x of
        First expr   -> Left (KErrorExpectedTagButGotExpr expr)
        Second type' -> Left (KErrorExpectedTagButGotTypeAnnotation type')
        Third tags   -> Right tags

extractType :: OneOf3 V.Expr V.TypeAnnotation [V.UnlinkedTag] -> Either KeliError V.TypeAnnotation
extractType x = 
    case x of
        First expr   -> Left (KErrorExpectedTypeAnnotButGotExpr expr)
        Second type' -> Right type' 
        Third tag    -> Left (KErrorExpectedTypeAnnotationButGotTag tag)



extractExpr :: OneOf3 V.Expr V.TypeAnnotation ([V.UnlinkedTag]) -> Either KeliError V.Expr
extractExpr x = 
    case x of 
        First expr -> Right expr
        Second type' -> Left (KErrorExpectedExprButGotTypeAnnotation type')
        Third tag    -> Left (KErrorExpectedExprButGotTag tag)


data UnverifiedBranch 
    = UnverifiedCarrylessTagBranch
        Raw.StringToken -- tag name
        Raw.Expr

    | UnverifiedPartialCarryfulTagBranch
        Raw.StringToken -- tag name
        Raw.StringToken -- carry binding variable

    | UnverifiedCarryfulTagBranch
        Raw.StringToken -- tag name
        Raw.StringToken -- carry binding variable
        Raw.Expr
    deriving (Show)

insertSymbolIntoEnv :: KeliSymbol -> Env -> Either KeliError Env
insertSymbolIntoEnv symbol env =
    case symbol of 
        KeliSymFunc [f] ->
            let funcid = (List.intercalate "$" (map snd (V.funcDeclIds f))) in
            let funcsWithSameName = lookup funcid env in
            let funcParamTypes = (\func -> map snd (V.funcDeclParams func)) in
            case funcsWithSameName of
                Just (KeliSymFunc fs) ->
                    -- check for duplication
                    if any 
                        (\func -> 
                            let allParamSignatureIsSame =
                                    length (V.funcDeclParams f) == length (V.funcDeclParams func)
                                    &&
                                    all 
                                        (\(t1,t2) -> 
                                            V.stringifyType (V.getTypeRef t1) == V.stringifyType (V.getTypeRef t2))
                                        (zip (funcParamTypes f) (funcParamTypes func)) in
                            
                            -- if is at the same location, means its for recursive function definition
                            let funcNameIsDeclaredAtTheSameLocation = 
                                    V.funcDeclIds f == V.funcDeclIds func in
                            
                            -- if all param signature is the same but name is not declared at same place,
                            -- then its a duplicate
                            allParamSignatureIsSame && not funcNameIsDeclaredAtTheSameLocation) 
                        fs then
                        Left (KErrorDuplicatedFunc f)
                    else
                        Right (env |> (funcid, KeliSymFunc (f:fs)))
                
                Just _ ->
                    Left (KErrorDuplicatedId (V.funcDeclIds f))

                Nothing ->
                    Right (env |> (funcid, symbol))

        KeliSymType t ->
            let typeId = 
                    case t of
                        V.TypeTaggedUnion (V.TaggedUnion name _ _ _)  -> 
                            name

                        V.TypeObject (Just name) _ ->
                            name

                        other ->
                            error (show other)
                            
            in
            insert' typeId

        KeliSymGlobalConst id _ ->
            insert' id

        KeliSymLocalConst id _ ->
            insert' id

        KeliSymTaggedUnion (V.TaggedUnion name _ _ _) ->
            insert' name

        other -> 
            error (show other)
            undefined

    where
        insert' id =
            if member (snd id) env then
                Left (KErrorDuplicatedId [id])
            else 
                Right (env |> (snd id, symbol))


verifyKeyValuePairs 
    :: Context 
    -> [Raw.StringToken]
    -> [Raw.Expr]
    -> Either KeliError (Context, [(V.StringToken, V.Expr)])
verifyKeyValuePairs ctx keys values = 
    case findDuplicates keys of
        Just duplicates ->
            Left (KErrorDuplicatedProperties duplicates)
        Nothing -> do
            (ctx2, typeCheckedExprs) <- verifyExprs ctx CanBeAnything values
            Right (ctx2, zip keys typeCheckedExprs)

verifyKeyTypeAnnotPairs 
    :: Context 
    -> [Raw.StringToken]
    -> [Raw.Expr]
    -> Either KeliError (Context, [(V.StringToken, V.TypeAnnotation)])
verifyKeyTypeAnnotPairs ctx keys types = do
    case findDuplicates keys of
        Just duplicates ->
            Left (KErrorDuplicatedProperties duplicates)
        Nothing -> do
            (ctx2, verifiedTypes) <- verifyTypeAnnotations ctx types
            Right (ctx2, zip keys verifiedTypes)


-- copied from https://stackoverflow.com/questions/49843681/getting-even-and-odd-position-of-elements-in-list-haskell-mutual-recursion/49844021
-- Retrieve even-indexed elements 
evens :: [a] -> [a]
evens (x:xs) = x:odds xs
evens _ = []

-- Retrieve odd-indexed elements
odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []

verifyTagname :: [V.Tag] -> V.StringToken -> Either KeliError (V.VerifiedTagname, V.Tag)
verifyTagname expectedTags actualTagname = 
    case List.find (\t -> snd (V.tagnameOf t) == snd actualTagname) expectedTags of
        Just tag -> 
            Right (V.VerifiedTagname actualTagname, tag)

        Nothing ->
            Left (KErrorUnknownTag actualTagname)

instantiateTypeVar :: Context -> [V.Type] -> (Context, Substitution)
instantiateTypeVar ctx boundedTypeVars =
    let (finalCtx, finalSubst) = 
            List.foldl' 
            (\(ctx', subst) nextT ->
                case nextT of
                    V.BoundedTypeVar (_, name) constraint ->
                        let (ctx'', tvar) = createNewTVar ctx' constraint in
                        (ctx'', Map.insert name tvar subst)

                    V.TypeTaggedUnion (V.TaggedUnion _ _ _ innerTypes) ->
                        instantiateTypeVar ctx' innerTypes

                    -- for other types, return the same substitution and context
                    _ ->
                        (ctx, subst))

            (ctx, emptySubstitution)
            boundedTypeVars
    in (finalCtx, finalSubst)

createNewTVar :: Context -> Maybe V.TypeConstraint -> (Context, V.Type)
createNewTVar (Context nextInt env importedEnvs) constraint =
    (Context (nextInt + 1) env importedEnvs, V.FreeTypeVar ("T$" ++ show nextInt) constraint)


typeCheckTagBranch 
    :: Context 
    -> [V.Tag]
    -> (Raw.Expr, Raw.Expr) 
    -> Either KeliError (Context, V.TagBranch)
typeCheckTagBranch ctx@(Context _ env importedEnvs) expectedTags (tag, branch) =
    case tag of
        -- carryful tag branch
        Raw.Lambda _ (Raw.FuncCall (_:(Raw.Id binding):[]) (actualTagname:[])) True -> do
            (verifiedTagname, verifiedTag) <- verifyTagname expectedTags actualTagname
            case verifiedTag of
                V.CarrylessTag{} -> 
                    Left (KErrorBindingCarrylessTag actualTagname)

                V.CarryfulTag _ expectedCarryType _ -> do
                    -- update env with binding
                    updatedEnv <- insertSymbolIntoEnv (KeliSymLocalConst binding expectedCarryType) env
                    (ctx2, typeCheckedBranch) <- typeCheckExpr (ctx{contextEnv = updatedEnv}) CanBeAnything branch 
                    typeCheckedBranch' <- extractExpr typeCheckedBranch
                    Right (ctx2, V.CarryfulTagBranch verifiedTagname binding typeCheckedBranch')

        -- carryless tag branch
        Raw.Lambda _ (Raw.FuncCall (_:[]) (actualTagname:[])) True -> do
            (verifiedTagname,_) <- verifyTagname expectedTags actualTagname
            (ctx2, typeCheckedBranch) <- typeCheckExpr ctx CanBeAnything branch 
            typeCheckedBranch' <- extractExpr typeCheckedBranch
            Right (ctx2, V.CarrylessTagBranch verifiedTagname typeCheckedBranch')
        
        other -> 
            Left (KErrorExpectedTagBindings other)


allBranchTypeAreSame 
    :: Context
    -> [V.TagBranch]
    -> Either 
        KeliError 
        V.Type -- type of the first branch

allBranchTypeAreSame ctx typeCheckedTagBranches = do
    let (firstBranch:tailBranches) = map (\b -> case b of 
            V.CarryfulTagBranch _ _ expr -> expr
            V.CarrylessTagBranch _ expr -> expr
            V.ElseBranch expr -> expr) typeCheckedTagBranches

    let expectedTypeOfEachBranches = getType firstBranch 
    case mapM (\b -> unify ctx emptySubstitution b expectedTypeOfEachBranches) tailBranches of
        Left err ->
            Left err

        Right _ ->
            Right expectedTypeOfEachBranches
                                        
lookupEnvs :: String -> Env -> [(ModuleName,Env)] -> [(V.Scope,KeliSymbol)]
lookupEnvs identifier currentEnv importedEnvs = 
    let symbolsFromCurrentScope = 
            case lookup identifier currentEnv of
                Just s ->
                    Just (V.FromCurrentScope, s)
                
                Nothing ->
                    Nothing 
    in
    let symbolsFromImportedFiles = 
            map 
                (\(modulename, env) ->
                    case lookup identifier env of
                        Just s ->
                            Just (V.FromImports modulename, s)
                        Nothing ->
                            Nothing) 
                importedEnvs 
    in
    catMaybes (symbolsFromCurrentScope:symbolsFromImportedFiles)


---------------------------------------------------------------------------------------
-- Unify functions
--      Why these functions are not moved to its own file Unify.hs? 
--      This is because unify and typeCheck depends on each other mutually (i.e. mutually recursive)
--      Thus, they cannot be separated into two different files
---------------------------------------------------------------------------------------

unifyMany 
    :: Context
    -> Substitution -- initial subst
    -> [V.Expr]  -- actual exprs (for reporting error location only)
    -> [V.Type]  -- expected types
    -> Either 
        KeliError 
        (Substitution, [V.Expr]) -- (subst, inferred exprs)

unifyMany ctx@(Context _ env _) subst1 exprs expectedTypes = do
    (subst2, exprs') <- unifyMany' ctx subst1 exprs expectedTypes

    -- perform second pass type checking, mainly for inferring lambda types
    let subst3 = composeSubst subst1 subst2
    inferredExprs <- 
            mapM 
                (\e -> 
                    case e of 
                    V.Expr 
                        (V.PartiallyInferredLambda paramName body) 
                        (V.TypeTaggedUnion (V.TaggedUnion _ _ _ [inputType, outputType])) -> do
                        let inputType' = applySubstitutionToType subst3 inputType 
                        let outputType' = applySubstitutionToType subst3 outputType 
                        updatedEnv <- insertSymbolIntoEnv (KeliSymLocalConst paramName inputType') env 
                        -- QUESTION: Should I ignore this new context?
                        (_, body') <- verifyExpr (ctx{contextEnv = updatedEnv}) CanBeAnything body
                        (tempSubst, body'') <- unify ctx subst3 body' outputType'
                        Right (V.Expr 
                            (V.Lambda (paramName, inputType') body'') 
                            (V.TypeTaggedUnion (newFunctionType inputType' (applySubstitutionToType tempSubst outputType'))))
                    _ -> 
                        Right e) (exprs')
    
    -- perform third pass type checking, mainly for inferring lambda types
    (subst4, inferredExprs') <- unifyMany' ctx subst3 inferredExprs expectedTypes 
    return (composeSubst subst3 subst4, inferredExprs') 


unifyMany' 
    :: Context
    -> Substitution 
    -> [V.Expr] 
    -> [V.Type] 
    -> Either KeliError (Substitution, [V.Expr])
unifyMany' ctx' subst' exprs' expectedTypes' = 
    foldM 
        (\(prevSubst, prevExprs) (actualExpr, expectedType) -> do
            -- apply previous substituion to current expectedParamType
            let expectedType' = applySubstitutionToType prevSubst expectedType
            (nextSubst, actualExpr') <- unify ctx' subst' actualExpr expectedType'
            Right (composeSubst prevSubst nextSubst, prevExprs ++ [actualExpr']))
        (subst', [])
        (zip exprs' expectedTypes')

getType :: V.Expr -> V.Type
getType (V.Expr _ t) = t

type Substitution = Map.Map String V.Type

emptySubstitution :: Map.Map String V.Type
emptySubstitution = Map.empty


-- why does UnifyResult needs to contains V.Expr?
-- Answer: It is the updated expression
--  When unifying PartiallyInferredLambda, the result might be a properly inferred lambda
type UnifyResult = Either KeliError (Substitution, V.Expr) 

unify 
    :: Context 
    -> Substitution
    -> V.Expr  -- actual expr (for reporting error location only)
    -> V.Type  -- expected type
    -> UnifyResult

-- unify type variables
unify _ _ expr@(V.Expr _ (V.FreeTypeVar name constraint)) t =
    unifyTVar expr name constraint t

unify _ _ expr@(V.Expr _ t) (V.FreeTypeVar name constraint) =
    unifyTVar expr name constraint t

-- unify named types
unify _ _ expr@(V.Expr _ (V.TypeFloat)) (V.TypeFloat) = 
    Right (emptySubstitution, expr)  

unify _ _ expr@(V.Expr _ (V.TypeInt)) (V.TypeInt) = 
    Right (emptySubstitution, expr)  

unify _ _ expr@(V.Expr _ V.TypeString) (V.TypeString) = 
    Right (emptySubstitution, expr)  

-- unify bounded type variables
unify _ _ 
    expr@(V.Expr actualExpr actualType@(V.BoundedTypeVar name1 _))
    expectedType@(V.BoundedTypeVar name2 _) = 
    if snd name1 == snd name2 then
        Right (emptySubstitution, expr)  
    else
        Left (KErrorTypeMismatch actualExpr actualType expectedType)

unify _ _
    expr@(V.Expr actualExpr actualType@(V.TypeObjectConstructor name1 _))
    expectedType@(V.TypeObjectConstructor name2 _) = 
    if name1 == name2 then
        Right (emptySubstitution, expr)
    else
        Left (KErrorTypeMismatch actualExpr actualType expectedType)

unify _ _ (V.Expr _ V.TypeType) V.TypeType = 
    undefined

-- unify' tagged union
unify ctx subst1
    expr@(V.Expr actualExpr actualType@(V.TypeTaggedUnion (V.TaggedUnion name1 _ _ actualInnerTypes)))
    expectedType@(V.TypeTaggedUnion (V.TaggedUnion name2 _ _ expectedInnerTypes)) = 
    if name1 == name2 && (length actualInnerTypes == length expectedInnerTypes) then do
        case 
            foldM 
                (\prevSubst (actualInnerType, expectedInnerType) -> do
                    let mockExpr = V.Expr actualExpr actualInnerType -- mockExpr is just for reporting error location
                    (nextSubst, _) <- unify ctx prevSubst mockExpr expectedInnerType
                    Right (composeSubst prevSubst nextSubst))
                subst1
                (zip actualInnerTypes expectedInnerTypes) of

            Right subst2 ->
                Right (subst2, expr)
            
            Left{} ->
                Left (KErrorTypeMismatch actualExpr actualType expectedType)
    else 
        Left (KErrorTypeMismatch actualExpr actualType expectedType)


-- unfify object type
-- object type is handled differently, because we want to have structural typing
-- NOTE: kts means "key-type pairs"
unify ctx subst expr@(V.Expr actualExpr (V.TypeObject _ kts1)) (V.TypeObject objectTypeName kts2) = 
    let (actualKeys, actualTypes) = unzip (List.sortOn (\((_,k),_) -> k) kts1) in
    let (expectedKeys, expectedTypes) = unzip  (List.sortOn (\((_,k),_) -> k) kts2) in
    -- TODO: get the set difference of expectedKeys with actualKeys
    -- because we want to do structural typing
    -- that means, it is acceptable if actualKeys is a valid superset of expectedKeys
    case match actualKeys expectedKeys of
        PerfectMatch -> do
            case actualExpr of 
                -- if actualExpr is an object literal
                V.Object keyValuePairs -> do
                    let (_, actualValues) = unzip (List.sortOn (\((_,k),_) -> k) (keyValuePairs))
                    (subst2, actualValues') <- unifyMany ctx subst actualValues expectedTypes
                    return (subst2, V.Expr 
                        (V.Object (zip actualKeys actualValues')) 
                        (V.TypeObject objectTypeName (zip actualKeys (map getType actualValues'))))

                -- if not
                _ -> do
                    subst2 <- foldM
                        (\prevSubst (key, actualType, expectedType) -> 

                            -- actualExpr is passed in so that the location of error can be reported properly
                            case unify ctx subst (V.Expr actualExpr actualType) (applySubstitutionToType prevSubst expectedType) of
                                Right (nextSubst, _) ->
                                    Right (composeSubst prevSubst nextSubst)

                                Left KErrorTypeMismatch{} ->
                                    Left (KErrorPropertyTypeMismatch key expectedType actualType actualExpr )

                                Left err ->
                                    Left err)
                        emptySubstitution
                        (zip3 actualKeys actualTypes expectedTypes)

                    return (subst2, expr)

        GotDuplicates duplicates ->
            Left (KErrorDuplicatedProperties duplicates)

        GotExcessive excessiveProps ->
            Left (KErrorExcessiveProperties excessiveProps)
        
        Missing missingProps ->
            Left (KErrorMissingProperties actualExpr missingProps)

        ZeroIntersection ->
            Left (KErrorMissingProperties actualExpr (map snd expectedKeys))
        

unify _ _ (V.Expr actualExpr actualType) expectedType =  Left (KErrorTypeMismatch actualExpr actualType expectedType)


unifyTVar :: V.Expr -> String -> Maybe V.TypeConstraint -> V.Type -> UnifyResult
unifyTVar expr tvarname1 _ t2 =
    -- NOTE: actualExpr is used for reporting error location only
    let result = Right (Map.insert tvarname1 t2 emptySubstitution, expr) in
    case t2 of
        V.FreeTypeVar tvarname2 _ ->
            if tvarname1 == tvarname2 then
                Right (emptySubstitution, expr)
            else
                result

        _ ->
            if t2 `contains` tvarname1 then
                Left (KErrorTVarSelfReferencing expr tvarname1 t2)

            else
                result 


contains :: V.Type -> String -> Bool
t `contains` tvarname = 
    case t of
        V.BoundedTypeVar (_,name) _ ->
            name == tvarname

        V.FreeTypeVar name _ ->
            name == tvarname

        V.TypeTaggedUnion (V.TaggedUnion _ _ _ types) ->
            any (`contains` tvarname) types

        _ ->
            False

{- 
    Composing substitution s1 and s1

     For example if 

        s1 = {t1 => Int, t3 => t2} 
        s2 = {t2 => t1}

    Then the result will be

        s3 = {
            t1 => Int,
            t2 => Int,
            t3 => Int
        }
-}
composeSubst :: Substitution -> Substitution -> Substitution 
composeSubst s1 s2 =
    let result = 
            foldl 
                (\subst (key, type') -> Map.insert key (applySubstitutionToType s1 type') subst) 
                emptySubstitution
                ((Map.assocs s2)::[(String, V.Type)]) in

    -- cannot be Map.union result s1
    -- because we want keys in result to override duplicates found in s1
    Map.union result s1


-- Replace the type variables in a type that are
-- present in the given substitution and return the
-- type with those variables with their substituted values
-- eg. Applying the substitution {"a": Bool, "b": Int}
-- to a type (a -> b) will give type (Bool -> Int)
applySubstitutionToType :: Substitution -> V.Type -> V.Type
applySubstitutionToType subst type' =
    case type' of
        V.FreeTypeVar name _ ->
            case Map.lookup name subst of
                Just t ->
                    t
                Nothing ->
                    type'

        V.BoundedTypeVar name _ ->
            case Map.lookup (snd name) subst of
                Just t ->
                    t
                Nothing ->
                    type'

        V.TypeTaggedUnion (V.TaggedUnion name ids tags innerTypes) ->
            let resultingInnerTypes = map (applySubstitutionToType subst) innerTypes
                resultingTaggedUnion = V.TaggedUnion name ids resultingTags resultingInnerTypes
                resultingTags = 
                        map 
                        (\x -> case x of
                            V.CarryfulTag name' expectedCarryType _ -> 
                                let updatedCarryType = 
                                        applySubstitutionToType subst (expectedCarryType) in

                                V.CarryfulTag name' updatedCarryType resultingTaggedUnion

                            V.CarrylessTag name' _ -> 
                                V.CarrylessTag name' resultingTaggedUnion) 
                        tags
            in
            V.TypeTaggedUnion resultingTaggedUnion

        V.TypeObject name propTypePairs ->
            let (props, types) = unzip propTypePairs in
            V.TypeObject name (zip props (map (applySubstitutionToType subst) types))
        
        other ->
            other
