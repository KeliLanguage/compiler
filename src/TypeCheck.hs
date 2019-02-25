{-# LANGUAGE BangPatterns #-}
module TypeCheck where

import Control.Monad
import Data.Maybe
import Data.List hiding (lookup)
import Data.Map.Ordered ((|>), lookup, member) 
import qualified Data.Map.Strict as Map
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError
import Env
import Util
import Unify

data Assumption 
    = StrictlyAnalyzingType
    | CanBeAnything 
    deriving (Show)

typeCheckExpr :: Context -> Assumption -> Raw.Expr -> Either KeliError (Context, OneOf3 V.Expr V.TypeAnnotation [V.UnlinkedTag])
typeCheckExpr ctx@(Context _ env importedEnvs) assumption expression = case expression of 
    Raw.IncompleteFuncCall expr positionOfTheDotOperator -> do
        (_,typeCheckedExpr) <- typeCheckExpr ctx assumption expr 
        Left (KErrorIncompleteFuncCall typeCheckedExpr positionOfTheDotOperator)

    Raw.NumberExpr(pos,n) -> 
        case n of 
            Left intValue ->
                Right (ctx, First (V.Expr (V.IntExpr (pos, intValue)) ( V.TypeInt)))

            Right doubleValue ->
                Right (ctx, First (V.Expr (V.DoubleExpr (pos, doubleValue)) ( V.TypeFloat)))

    Raw.StringExpr (pos, str) -> 
        Right (ctx, First (V.Expr (V.StringExpr (pos, str)) ( V.TypeString)))

    Raw.Id token@(_,id) -> do
        lookupResult <- lookupEnvs [token] id env importedEnvs
        case lookupResult of 
            Just (KeliSymConst _ type') -> 
                Right (ctx, First (V.Expr (V.Id token) type'))
        
            Just (KeliSymType t@(V.TypeRecord name propTypePairs)) -> 
                -- Question: How are we gonna know if user is using this as type annotation or as record constructor?
                -- Answer: Using assumptions
                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (ctx, Second (V.TypeAnnotSimple token t)))

                            
                    CanBeAnything ->
                        (Right (ctx, First (V.Expr (V.RecordConstructor name propTypePairs) ((V.TypeRecordConstructor name propTypePairs)))))

            Just (KeliSymType t@((V.TypeTaggedUnion (V.TaggedUnion name _ tags innerTypes)))) ->
                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (ctx, Second (V.TypeAnnotSimple token t)))
                    CanBeAnything ->
                        (Right (ctx, First (V.Expr (V.TagConstructorPrefix name) ( (V.TypeTagConstructorPrefix name tags innerTypes)))))
            
            Just (KeliSymType t) ->
                (Right (ctx, Second (V.TypeAnnotSimple token t)))
            
            Just (KeliSymTaggedUnion t@(V.TaggedUnion _ _ tags typeParams)) ->
                let name = token in
                case assumption of
                    StrictlyAnalyzingType ->
                        if length typeParams > 0 then
                            Right (ctx, First (V.Expr (V.TypeConstructorPrefix name) ((V.TypeTypeConstructor t))))
                        else
                            Right (ctx, Second (V.TypeAnnotSimple name (V.TypeTaggedUnion t)))

                    CanBeAnything -> 
                        (Right (ctx, First (V.Expr (V.TagConstructorPrefix name) ( (V.TypeTagConstructorPrefix name tags typeParams)))))

            Nothing -> 
                Left (KErrorUsingUndefinedId token)
            
            other -> 
                error (show other)
                undefined

    Raw.FuncCall params' funcIds -> do
        case (head params') of
            (Raw.Id firstParamToken@(_,firstParamId)) -> 
                case firstParamId of
                -- 1. Check if user wants to create a tagged union
                "tags" ->
                    case find (\(_,x) -> x /= "case") funcIds of
                        Just x ->
                            Left (KErrorExpectedKeywordCase x)
                        Nothing -> do
                            (ctx2, tags) <- foldM 
                                    (\(prevCtx, prevTags) currentExpr -> 
                                        case currentExpr of
                                            Raw.Id id ->
                                                Right (prevCtx, prevTags ++ [V.UnlinkedCarrylessTag id])

                                            Raw.FuncCall params'' funcIds'' ->
                                                if length params'' < 2 then
                                                    Left (KErrorExpectedTypeAnnotationAfterThis (last funcIds''))
                                                else do
                                                    case head params'' of
                                                        Raw.Id tagname -> do
                                                            let keys = funcIds''
                                                            (ctx2, keyTypePairs) <- verifyKeyTypeAnnotPairs ctx keys (tail params'')
                                                            Right (ctx2, prevTags ++ [V.UnlinkedCarryfulTag tagname keyTypePairs])

                                                        other ->
                                                            Left (KErrorExpectedId other)
                                            _ ->
                                                Left (KErrorExpectedPropDefOrId currentExpr)) 
                                    (ctx, [])
                                    (tail params')
                            Right (ctx2, Third tags)

                -- 2. Check if the user wants to create a record (type/value)
                "record" ->  
                    if length (tail params') == 0 then 
                        Left (KErrorIncorrectUsageOfRecord firstParamToken)

                    else do 
                        -- NOTE: 
                        --  Because of the following line of code,
                        --  the following recursive record type cannot be declared:
                        --
                        --      fruit = record.next fruit;
                        -- 
                        -- It's ok, because we shouldn't allow user to create recursive records (which will form infinite type)
                        (ctx2, firstValue) <- typeCheckExpr ctx assumption (tail params' !! 0)  -- <-- this line

                        let keys = funcIds
                        case firstValue of 
                            -- assume user want to create a record value
                            First _ -> do
                                (ctx3, keyValuePairs) <- verifyKeyValuePairs ctx2 keys (tail params')
                                Right 
                                    (ctx3, First
                                        (V.Expr
                                            (V.Record keyValuePairs) 
                                            ( (V.TypeRecord Nothing (map (\(k, expr) -> (k, getType expr)) keyValuePairs)))))
                            
                            -- assume user want to declare a record type
                            Second _ -> do
                                (ctx3, keyTypePairs) <- verifyKeyTypeAnnotPairs ctx keys (tail params')
                                Right (ctx3, Second 
                                    (V.TypeAnnotCompound 
                                        firstParamToken 
                                        keyTypePairs 
                                        (V.TypeRecord Nothing (map (\(k, ta) -> (k, V.getTypeRef ta)) keyTypePairs))))

                            Third tag -> 
                                Left (KErrorExpectedExprOrTypeButGotTag tag)

                -- 3. check if user is using javascript ffi
                "ffi" ->
                    if length funcIds /= 1 || length params' /= 2 then
                        Left (KErrorIncorrectUsageOfFFI firstParamToken)
                    else if snd (funcIds !! 0) /= "javascript" then
                        Left (KErrorUnknownFFITarget (funcIds !! 0))
                    else do
                        (ctx2, jsCode) <- typeCheckExpr ctx assumption (params' !! 1)
                        jsCode' <- extractExpr jsCode
                        case jsCode' of
                            (V.Expr (V.StringExpr value) _) ->
                                Right (ctx2, First (V.Expr (V.FFIJavascript value) ( V.TypeUndefined)))

                            _ -> 
                                Left (KErrorFFIValueShouldBeString jsCode')
                    
                _ -> 
                    continuePreprocessFuncCall1

            _ -> 
                continuePreprocessFuncCall1

        where 
            continuePreprocessFuncCall1 :: Either KeliError (Context, OneOf3 V.Expr V.TypeAnnotation [V.UnlinkedTag])
            continuePreprocessFuncCall1 = do
                (ctx2, firstParam) <- verifyExpr ctx assumption  (head params')

                let typeOfFirstParam = getType firstParam in
                    case typeOfFirstParam of
                        -- (A) check if user is invoking record constructor
                        V.TypeRecordConstructor aliasedName expectedPropTypePairs -> do
                            (ctx3, values)  <- verifyExprs ctx2 assumption (tail params')
                            let actualPropValuePairs = zip funcIds values
                            let actualRecord = V.Expr (V.Record actualPropValuePairs) (V.TypeRecord aliasedName (zip funcIds (map getType values)))
                            substitution <- unify actualRecord (V.TypeRecord aliasedName expectedPropTypePairs)
                            let resultingType = applySubstitutionToType substitution (V.TypeRecord aliasedName expectedPropTypePairs)
                            Right (ctx3, First (V.Expr (V.Record actualPropValuePairs) resultingType))

                        -- (B) check if user is performing function application
                        V.TypeTaggedUnion (V.TaggedUnion (_,"Function") _ _ innerTypes) ->
                            if snd (head funcIds) == "apply" && length (tail params') == 1 then do
                                (ctx3, value) <- verifyExpr ctx2 assumption (last params')
                                let f = firstParam
                                Right (ctx3, First(V.Expr (V.FuncApp f value) (innerTypes !! 1)))
                            else
                                continuePreprocessFuncCall2

                        -- (C) check if user is calling tag matchers
                        V.TypeTaggedUnion (V.TaggedUnion _ _ expectedTags _) -> do
                            let subject = firstParam 
                            case find (\(_,x) -> x == "if") funcIds of
                                Nothing ->
                                    continuePreprocessFuncCall2

                                Just _ -> do
                                    -- 0. take out else branch
                                    let (rawElseBranches, noElseBranches) = partition (\((_,x),_) -> x == "else:") (zip funcIds (tail params'))
                                    let (elselessFuncIds, elselessParams) = unzip noElseBranches

                                    -- 1. check if syntax is correct
                                    !_ <- mapM (\(pos,x) -> if x == "if" || x == "else" then Right() else Left (KErrorExpectedIfOrElse (pos,x))) (evens elselessFuncIds)
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
                                    (ctx3, typeCheckedElseBranches) <- verifyExprs ctx2 assumption (map snd rawElseBranches)

                                    -- 3. check if each branch have the same type with the first branch
                                    case allBranchTypeAreSame (typeCheckedTagBranches ++ map (V.ElseBranch) typeCheckedElseBranches) of
                                        Left err ->
                                            case err of
                                                KErrorTypeMismatch actualExpr actualType expectedType ->
                                                    Left (KErrorNotAllBranchHaveTheSameType 
                                                        actualExpr actualType expectedType (head typeCheckedTagBranches))

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

                                                Missing missingTags ->
                                                    case length typeCheckedElseBranches of
                                                        0 ->
                                                            Left (KErrorMissingTags subject missingTags)

                                                        1 ->
                                                            let elseBranch = head typeCheckedElseBranches in
                                                            Right (ctx3, First (V.Expr 
                                                                (V.TagMatcher subject typeCheckedTagBranches (Just elseBranch)) 
                                                                branchType))

                                                        _ ->
                                                            Left (KErrorMoreThanOneElseBranch (map fst rawElseBranches))

                                                other ->
                                                    error (show other)
                                                    -- error "Shoudln't reach here, because `typeCheckTagBranch` already check for unknown tags"


                        -- (D) check if user is calling record getter/setter
                        recordType@(V.TypeRecord _ kvs) ->  
                            if length funcIds > 1 then
                                continuePreprocessFuncCall2
                            else
                                let subject = firstParam in
                                let actualPropertyName = funcIds !! 0 in
                                case find (\((_,key),_) -> key == snd actualPropertyName) kvs of
                                    Just (_, expectedType) -> 
                                        -- Check if is getter or setter
                                        if length (tail params') == 0 then -- is getter
                                            Right (ctx, First (V.Expr (V.RecordGetter subject actualPropertyName) expectedType))
                                        else if length (tail params') == 1 then -- is setter
                                            case last params' of
                                                -- if is lambda setter
                                                -- TODO: need to check if expectedType is Function
                                                -- something squishy is happening
                                                Raw.Lambda lambdaParam lambdaBody -> do
                                                    updatedEnv <- insertSymbolIntoEnv (KeliSymConst lambdaParam expectedType) env 
                                                    (ctx3, verifiedLambdaBody) <- verifyExpr (ctx2{contextEnv = updatedEnv}) assumption lambdaBody

                                                    substitution <- unify verifiedLambdaBody expectedType
                                                    let returnType = applySubstitutionToType substitution recordType
                                                    Right (ctx3, First (V.Expr 
                                                        (V.RecordLambdaSetter 
                                                            subject
                                                            actualPropertyName
                                                            lambdaParam
                                                            verifiedLambdaBody)
                                                        (returnType)))

                                                -- else is value setter
                                                _ -> do
                                                    (ctx3, newValue) <- verifyExpr ctx assumption ((tail params') !! 0)
                                                    substitution <- unify newValue expectedType
                                                    let returnType = applySubstitutionToType substitution recordType
                                                    Right (ctx3, First (V.Expr 
                                                        (V.RecordSetter subject actualPropertyName newValue) 
                                                        (returnType)))
                                        else
                                            continuePreprocessFuncCall2

                                    Nothing -> 
                                        continuePreprocessFuncCall2

                        -- (E) check if user is invoking tag constructor prefix
                        V.TypeTagConstructorPrefix taggedUnionName tags _ ->
                            if length funcIds == 1 then
                                let tagname = head funcIds in
                                case find (\t -> snd (V.tagnameOf t) == snd tagname) tags of
                                    -- if is carryless tag
                                    Just (V.CarrylessTag tag (V.TaggedUnion name ids _ innerTypes)) -> 
                                        let (ctx3, subst) = instantiateTypeVar ctx innerTypes in
                                        let resultingInnerTypes = map (applySubstitutionToType subst) innerTypes in
                                        let belongingUnion = V.TaggedUnion name ids tags resultingInnerTypes in
                                        (Right (ctx3, First (V.Expr 
                                            (V.CarrylessTagExpr tag tagname) 
                                            ((V.TypeTaggedUnion belongingUnion)))))

                                    -- if is carryful tag
                                    Just (V.CarryfulTag _ expectedPropTypePairs belongingUnion) ->
                                        (Right (ctx2, First (V.Expr
                                            (V.CarryfulTagConstructor tagname expectedPropTypePairs)
                                            ((V.TypeCarryfulTagConstructor 
                                                tagname
                                                expectedPropTypePairs
                                                belongingUnion)))))
                                    Nothing ->
                                        Left (KErrorTagNotFound tagname taggedUnionName tags)

                            else
                                Left (KErrorIncorrectUsageOfTagConstructorPrefix expression)

                        -- (F) check if user is invoking type constructor
                        V.TypeTypeConstructor (V.TaggedUnion name ids tags _) ->
                            if map snd funcIds /= map snd ids then
                                Left (KErrorTypeConstructorIdsMismatch ids funcIds)
                            else do
                                -- TODO: check if type param conforms to type constraint
                                (ctx3, types) <- verifyTypeAnnotationAnnotations ctx (tail params')
                                Right (ctx3, Second 
                                    (V.TypeAnnotCompound
                                        name
                                        (zip ids types)
                                        (V.TypeTaggedUnion (V.TaggedUnion name ids tags (map V.getTypeRef types)))))

                        -- (G) check if user is invoking carryful tag constructor
                        V.TypeCarryfulTagConstructor tagname expectedPropTypePairs (V.TaggedUnion name ids tags innerTypes) -> do
                            let (ctx3, subst1) = instantiateTypeVar ctx innerTypes 
                            (ctx4, values) <- verifyExprs ctx3 assumption (tail params')
                            let actualPropValuePairs = zip funcIds values
                            let actualRecord = V.Expr (V.Record actualPropValuePairs) (V.TypeRecord Nothing (zip funcIds (map getType values)))
                            let expectedPropTypePairs' = map (\(prop, t) -> (prop, applySubstitutionToType subst1 t)) expectedPropTypePairs
                            subst2 <- unify actualRecord (V.TypeRecord Nothing expectedPropTypePairs')

                            let innerTypes' = map (applySubstitutionToType subst1) innerTypes
                            let resultingInnerTypes = map (applySubstitutionToType subst2) innerTypes'
                            Right (ctx4, First (V.Expr 
                                (V.CarryfulTagExpr tagname actualPropValuePairs)
                                (V.TypeTaggedUnion (V.TaggedUnion name ids tags resultingInnerTypes))))

                        -- otherwise
                        _ ->
                            continuePreprocessFuncCall2

                    
            
            continuePreprocessFuncCall2 = 
                case funcIds !! 0 of
                    -- check if user is calling type casting
                    (_, "as") ->
                        if length funcIds == 1 && length params' == 2 then do
                            (ctx2, subject) <- typeCheckExpr ctx assumption (params' !! 0)
                            subject' <- extractExpr subject
                            (ctx3, castType) <- typeCheckExpr ctx2 StrictlyAnalyzingType (params' !! 1)
                            castType' <- extractType castType
                            case subject' of
                                (V.Expr expr' ( V.TypeUndefined)) ->
                                    Right (ctx3, First (V.Expr expr' (V.getTypeRef castType')))

                                _ ->
                                    undefined
                        else
                            treatAsNormalFuncCall
                    
                    -- otherwise
                    _ -> 
                        treatAsNormalFuncCall

                        
            treatAsNormalFuncCall = do
                (ctx2, params) <- verifyExprs ctx assumption params'
                (ctx3, result) <- typeCheckFuncCall ctx2 assumption params funcIds
                Right (ctx3, First result)

    Raw.Lambda param body ->
        let (ctx2, inputTVar) = createNewTVar ctx Nothing in
        let (ctx3, outputTVar) = createNewTVar ctx2 Nothing in
        Right (ctx3, First (V.Expr 
            (V.PartiallyInferredLambda param body)
            (V.TypeTaggedUnion (newFunctionType inputTVar outputTVar))))


verifyTypeAnnotation :: Context -> Raw.Expr -> Either KeliError (Context, V.TypeAnnotation) 
verifyTypeAnnotation ctx rawExpr = do
    (ctx2, expr) <- typeCheckExpr ctx StrictlyAnalyzingType rawExpr
    type' <- extractType expr 
    Right (ctx2, type')

verifyTypeAnnotationAnnotations :: Context -> [Raw.Expr] -> Either KeliError (Context, [V.TypeAnnotation]) 
verifyTypeAnnotationAnnotations ctx rawExprs = do
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

verifyExprs :: Context -> Assumption -> [Raw.Expr] -> Either KeliError (Context, [V.Expr]) 
verifyExprs ctx assumption rawExprs =
    foldM 
        (\(prevCtx, verifiedExprs) nextRawExpr -> do
            (nextCtx, verifiedExpr) <- verifyExpr prevCtx assumption nextRawExpr
            Right (nextCtx, verifiedExprs ++ [verifiedExpr]))
        (ctx, [])
        rawExprs

data MatchFuncResult 
    = PerfectlyMatchedFuncFound 
        Context    -- updated context
        V.Expr     -- resulting func call expr

    | PartiallyMatchedFuncFound -- means the 2nd params onward does not match expected types
        KeliError  -- corresponding error (should be type mismatch error)

    | StillNoMatchingFunc

-- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
typeCheckFuncCall 
    :: Context 
    -> Assumption 
    -> [V.Expr] 
    -> [Raw.StringToken] 
    -> Either KeliError (Context, V.Expr)

typeCheckFuncCall ctx@(Context _ env importedEnvs) assumption funcCallParams funcIds = do
    lookupResult <- lookupEnvs funcIds (intercalate "$" (map snd funcIds)) env importedEnvs
    case lookupResult of
        Just (KeliSymFunc candidateFuncs) -> do
            case (foldl 
                    (\result currentFunc ->
                        case result of
                        p@PerfectlyMatchedFuncFound{} -> p

                        p@PartiallyMatchedFuncFound{} -> p
                        
                        -- if
                        StillNoMatchingFunc -> -- then continue the loop for searching matching functions
                            if length funcCallParams /= length (V.funcDeclParams currentFunc) then
                                StillNoMatchingFunc
                            else do
                                -- (A) instantiate type variables  
                                let (ctx2, subst1) = instantiateTypeVar ctx (V.funcDeclGenericParams currentFunc)

                                -- (B) apply substitution to every expected func param types
                                let expectedParamTypes = map (\(_,paramTypeAnnot) -> 
                                        applySubstitutionToType subst1 (V.getTypeRef paramTypeAnnot)) (V.funcDeclParams currentFunc) 

                                -- (C) check if the firstParam matches the firstParamExpectedType
                                case unify (head funcCallParams) (head expectedParamTypes) of
                                    -- if matches, continue to unify the following params
                                    Right subst2 -> 
                                        case unifyMany subst2 (tail funcCallParams) (tail expectedParamTypes) of
                                            Left err ->
                                                PartiallyMatchedFuncFound err

                                            Right subst3 -> 
                                                -- perform second pass type checking, mainly for inferring lambda types
                                                let subst4 = composeSubst subst2 subst3 in
                                                case mapM 
                                                        (\e -> 
                                                            case e of 
                                                            V.Expr 
                                                                (V.PartiallyInferredLambda paramName body) 
                                                                (V.TypeTaggedUnion (V.TaggedUnion _ _ _ [inputType, outputType])) -> do
                                                                let inputType' = applySubstitutionToType subst4 inputType 
                                                                let outputType' = applySubstitutionToType subst4 outputType 
                                                                updatedEnv <- insertSymbolIntoEnv (KeliSymConst paramName inputType') env 
                                                                -- QUESTION: Should I ignore this new context?
                                                                (_, body') <- verifyExpr (ctx2{contextEnv = updatedEnv}) assumption body
                                                                tempSubst <- unify body' outputType'
                                                                Right (V.Expr 
                                                                    (V.Lambda (paramName, inputType') body') 
                                                                    (V.TypeTaggedUnion (newFunctionType inputType' (applySubstitutionToType tempSubst outputType'))))
                                                            _ -> 
                                                                Right e) funcCallParams of
                                                    Right funcCallParams' -> 
                                                        let expectedReturnType = applySubstitutionToType subst1 (V.funcDeclReturnType currentFunc) in
                                                        -- unify again to get the best substitution (mainly for inferring lambdas)
                                                        case unifyMany subst4 funcCallParams' expectedParamTypes of
                                                            Left err ->
                                                                PartiallyMatchedFuncFound err
                                                            
                                                            Right subst5 ->
                                                                let expectedReturnType' = applySubstitutionToType subst5 expectedReturnType in
                                                                let funcCall = V.Expr (V.FuncCall funcCallParams' funcIds currentFunc) (expectedReturnType') in
                                                                PerfectlyMatchedFuncFound ctx2 funcCall
                                                    
                                                    Left err ->
                                                        PartiallyMatchedFuncFound err


                                    -- if not, 
                                    Left _ ->
                                        StillNoMatchingFunc)

                    
                        StillNoMatchingFunc

                        -- This sorting is necessary so that the compiler will look for more specialized (a.k.a less generic) function first
                        (sortOn (\f -> length (V.funcDeclGenericParams f)) candidateFuncs)) of

                PerfectlyMatchedFuncFound newCtx funcCallExpr ->
                    Right (newCtx, funcCallExpr)

                PartiallyMatchedFuncFound err ->
                    Left err

                StillNoMatchingFunc ->
                    Left (KErrorUsingUndefinedFunc funcIds candidateFuncs)
        
        Just _ ->
            Left (KErrorNotAFunction funcIds)

        _ -> 
            Left (KErrorUsingUndefinedFunc funcIds [])

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



extractExpr :: OneOf3 V.Expr V.TypeAnnotation [V.UnlinkedTag] -> Either KeliError V.Expr
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
            let funcid = (intercalate "$" (map snd (V.funcDeclIds f))) in
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

                        V.TypeRecord (Just name) _ ->
                            name

                        other ->
                            error (show other)
                            
            in
            insert' typeId

        KeliSymConst id _ ->
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
            (ctx2, verifiedTypes) <- verifyTypeAnnotationAnnotations ctx types
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
    case find (\t -> snd (V.tagnameOf t) == snd actualTagname) expectedTags of
        Just tag -> 
            Right (V.VerifiedTagname actualTagname, tag)

        Nothing ->
            Left (KErrorUnknownTag actualTagname)

instantiateTypeVar :: Context -> [V.Type] -> (Context, Substitution)
instantiateTypeVar ctx boundedTypeVars =
    let (finalCtx, finalSubst) = 
            foldl' 
            (\(ctx', subst) nextT ->
                case nextT of
                    V.BoundedTypeVar (_, name) constraint ->
                        let (ctx'', tvar) = createNewTVar ctx' constraint in
                        (ctx'', Map.insert name tvar subst)

                    other ->
                        error (show other))
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
        Raw.Id actualTagname -> do
            (verifiedTagname,_) <- verifyTagname expectedTags actualTagname
            (ctx2, typeCheckedBranch) <- typeCheckExpr ctx CanBeAnything branch 
            typeCheckedBranch' <- extractExpr typeCheckedBranch
            Right (ctx2, V.CarrylessTagBranch verifiedTagname typeCheckedBranch')


        
        Raw.FuncCall funcParams' funcIds' ->
            case head funcParams' of
                Raw.Id actualTagname -> do
                    (verifiedTagname, matchingTag) <- verifyTagname expectedTags actualTagname
                    case matchingTag of
                        (V.CarrylessTag{}) ->
                            Left (KErrorBindingCarrylessTag actualTagname)
                
                        (V.CarryfulTag _ expectedKeyTypePairs _) -> do
                            -- verify property bindings
                            let propBindings = zip funcIds' (tail funcParams')
                            verifiedPropBindings <- 
                                mapM 
                                    (\(actualProp, bindingExpr) -> 
                                        case find (\((_,p),_) -> p == snd actualProp) expectedKeyTypePairs of
                                            Just (_, expectedType) ->
                                                case bindingExpr of 
                                                    Raw.Id bindingId -> 
                                                        Right (actualProp, bindingId, expectedType)

                                                    other -> 
                                                        Left (KErrorExpectedId other)
                                            Nothing ->
                                                Left(KErrorUnknownProp actualProp))

                                    (propBindings)
                            
                            -- update env with property-bindings
                            updatedEnv <- 
                                foldM 
                                    (\prevSymtab (_,bindingId, expectedType) -> 
                                        insertSymbolIntoEnv (KeliSymConst bindingId expectedType) prevSymtab)
                                    env
                                    verifiedPropBindings

                            -- type check the branch
                            (ctx2, typeCheckedBranch) <- typeCheckExpr (ctx{contextEnv = updatedEnv}) CanBeAnything branch 
                            typeCheckedBranch' <- extractExpr typeCheckedBranch
                            Right (ctx2, V.CarryfulTagBranch verifiedTagname verifiedPropBindings typeCheckedBranch')

                other ->
                    Left (KErrorExpectedId other)

                        
        other -> 
            Left (KErrorExpectedTagBindings other)


allBranchTypeAreSame 
    :: [V.TagBranch] 
    -> Either 
        KeliError 
        V.Type -- type of the first branch
allBranchTypeAreSame typeCheckedTagBranches = do
    let branches = map (\b -> case b of 
            V.CarryfulTagBranch _ _ expr -> expr
            V.CarrylessTagBranch _ expr -> expr
            V.ElseBranch expr -> expr) typeCheckedTagBranches

    let firstBranch = head branches 
    let expectedTypeOfEachBranches = getType firstBranch 
    case mapM (\b -> unify b expectedTypeOfEachBranches) (tail branches) of
        Left err ->
            Left err

        Right _ ->
            Right expectedTypeOfEachBranches
                                        
lookupEnvs :: [V.StringToken] -> String -> Env -> [Env] -> Either KeliError (Maybe KeliSymbol)
lookupEnvs actualIds identifier currentEnv importedEnvs = 
    let result = map (lookup identifier) (currentEnv:importedEnvs) in
    let symbols = catMaybes result in
    if length symbols <= 0 then
        Right Nothing
    else if length symbols == 1 then
        Right (Just (symbols !! 0))
    else -- if length symbols > 1 then
        Left (KErrorAmbiguousUsage actualIds symbols)
    