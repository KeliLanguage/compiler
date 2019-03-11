{-# LANGUAGE BangPatterns #-}
module TypeCheck where

import Control.Monad
import Data.Char
import Data.Either
import Data.Maybe
import Data.List hiding (lookup)
import Module
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

    Raw.Array exprs ->
        case exprs of
            [] ->
                undefined

            x:xs -> do
                (ctx2, headElement) <- verifyExpr ctx assumption x 
                (ctx3, tailElements) <- verifyExprs ctx2 assumption xs
                let typeOfFirstElem = getType headElement
                -- make sure every element have the same type as the first element 
                !_ <- forM tailElements (\el -> unify el typeOfFirstElem)
                Right (ctx3, First (V.Expr
                    (V.Array (headElement:tailElements))
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
                
    Raw.FuncCall params' funcIds -> do
        case (head params') of
            -- 1. Check if user wants to create a tagged union
            (Raw.Id (_,"tags")) -> 
                case find (\(_,x) -> x /= "case") funcIds of
                    Just x ->
                        Left (KErrorExpectedKeywordCase x)
                    Nothing -> do
                        (ctx2, tags) <- foldM 
                                (\(prevCtx, prevTags) currentExpr -> 
                                    case currentExpr of
                                        Raw.Lambda _ f isShortHand ->
                                            if isShortHand then
                                                case f of
                                                    Raw.FuncCall (_:[]) (tagname:[]) ->
                                                        Right (prevCtx, prevTags ++ [V.UnlinkedCarrylessTag tagname])

                                                    Raw.FuncCall (_:secondParam:[]) (tagname:[]) -> do
                                                        (ctx2, carryType) <- verifyTypeAnnotation prevCtx secondParam
                                                        Right (ctx2, prevTags ++ [V.UnlinkedCarryfulTag tagname carryType])
                                                    
                                                    _ ->
                                                        Left (KErrorIncorrectTagDeclSyntax currentExpr)
                                                    
                                            else
                                                Left (KErrorIncorrectTagDeclSyntax currentExpr)
                                        
                                        _ ->
                                            Left (KErrorIncorrectTagDeclSyntax currentExpr))

                                (ctx, [])
                                (tail params')
                        Right (ctx2, Third tags)

            -- 2. Check if the user wants to create a object (type/value)
            (Raw.Id firstParamToken@(_,"$")) -> 
                if length (tail params') == 0 then 
                    Left (KErrorIncorrectUsageOfObject firstParamToken)

                else do 
                    -- NOTE: 
                    --  Because of the following line of code,
                    --  the following recursive object type cannot be declared:
                    --
                    --      fruit = object.next fruit;
                    -- 
                    -- It's ok, because we shouldn't allow user to create recursive objects (which will form infinite type)
                    (ctx2, firstValue) <- typeCheckExpr ctx assumption (tail params' !! 0)  -- <-- this line

                    let keys = funcIds
                    case firstValue of 
                        -- assume user want to create a object value
                        First _ -> do
                            (ctx3, keyValuePairs) <- verifyKeyValuePairs ctx2 keys (tail params')
                            Right 
                                (ctx3, First
                                    (V.Expr
                                        (V.Object keyValuePairs) 
                                        ( (V.TypeObject Nothing (map (\(k, expr) -> (k, getType expr)) keyValuePairs)))))
                        
                        -- assume user want to declare a object type
                        Second _ -> do
                            (ctx3, keyTypePairs) <- verifyKeyTypeAnnotPairs ctx keys (tail params')
                            Right (ctx3, Second (V.TypeAnnotObject keyTypePairs))

                        Third tag -> 
                            Left (KErrorExpectedExprOrTypeButGotTag tag)

            -- 3. check if user is using javascript ffi
            (Raw.Id firstParamToken@(_,"ffi")) -> 
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

        where 
            continuePreprocessFuncCall1 :: Either KeliError (Context, OneOf3 V.Expr V.TypeAnnotation [V.UnlinkedTag])
            continuePreprocessFuncCall1 = do
                (ctx2, firstParam) <- verifyExpr ctx assumption  (head params')

                let typeOfFirstParam = getType firstParam in
                    case typeOfFirstParam of
                        -- (A) check if user is invoking object constructor
                        V.TypeObjectConstructor aliasedName expectedPropTypePairs -> do
                            (ctx3, values)  <- verifyExprs ctx2 assumption (tail params')
                            let actualPropValuePairs = zip funcIds values
                            let actualObject = V.Expr (V.Object actualPropValuePairs) (V.TypeObject aliasedName (zip funcIds (map getType values)))
                            substitution <- unify actualObject (V.TypeObject aliasedName expectedPropTypePairs)
                            let resultingType = applySubstitutionToType substitution (V.TypeObject aliasedName expectedPropTypePairs)
                            Right (ctx3, First (V.Expr (V.Object actualPropValuePairs) resultingType))

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
                            case find (\(_,x) -> x == "case") funcIds of
                                Nothing ->
                                    continuePreprocessFuncCall2

                                Just _ -> do
                                    -- 0. take out else branch
                                    let (rawElseBranches, noElseBranches) = partition (\((_,x),_) -> x == "default:") (zip funcIds (tail params'))
                                    let (elselessFuncIds, elselessParams) = unzip noElseBranches

                                    -- 1. check if syntax is correct
                                    !_ <- mapM (\(pos,x) -> if x == "case" || x == "default" then Right() else Left (KErrorExpectedKeywordCaseOrDefault (pos,x))) (evens elselessFuncIds)
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

                                                Missing missingTagnames ->
                                                    case length typeCheckedElseBranches of
                                                        0 ->
                                                            let missingTags = 
                                                                    filter 
                                                                        (\t -> snd (V.tagnameOf t) `elem` missingTagnames)
                                                                        expectedTags

                                                            in Left (KErrorMissingTags subject missingTags)

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


                        -- (D) check if user is calling object getter/setter
                        objectType@(V.TypeObject _ kvs) ->  
                            if length funcIds > 1 then
                                continuePreprocessFuncCall2
                            else
                                let subject = firstParam in
                                let actualPropertyName = funcIds !! 0 in
                                case find (\((_,key),_) -> key == snd actualPropertyName) kvs of
                                    Just (_, expectedType) -> 
                                        -- Check if is getter or setter
                                        if length (tail params') == 0 then -- is getter
                                            Right (ctx, First (V.Expr (V.ObjectGetter subject actualPropertyName) expectedType))
                                        else if length (tail params') == 1 then -- is setter
                                            -- instantiate type vars for expectedType
                                            let (ctx3, subst1) = instantiateTypeVar ctx2 [expectedType] in
                                            let expectedType' = applySubstitutionToType subst1 expectedType in
                                            case last params' of
                                                -- if is lambda setter
                                                -- TODO: need to check if expectedType is Function
                                                -- something squishy is happening
                                                Raw.Lambda lambdaParam lambdaBody _ -> do
                                                    updatedEnv <- insertSymbolIntoEnv (KeliSymLocalConst lambdaParam expectedType') env 
                                                    (ctx4, verifiedLambdaBody) <- verifyExpr (ctx3{contextEnv = updatedEnv}) assumption lambdaBody

                                                    subst2 <- unify verifiedLambdaBody expectedType'
                                                    let returnType = applySubstitutionToType subst2 objectType
                                                    Right (ctx4, First (V.Expr 
                                                        (V.ObjectLambdaSetter 
                                                            subject
                                                            actualPropertyName
                                                            lambdaParam
                                                            verifiedLambdaBody)
                                                        (returnType)))

                                                -- else is value setter
                                                _ -> do
                                                    (ctx4, newValue) <- verifyExpr ctx3 assumption ((tail params') !! 0)
                                                    substitution <- unify newValue expectedType'
                                                    let returnType = applySubstitutionToType substitution objectType
                                                    Right (ctx3, First (V.Expr 
                                                        (V.ObjectSetter subject actualPropertyName newValue) 
                                                        (returnType)))
                                        else
                                            continuePreprocessFuncCall2

                                    Nothing -> 
                                        continuePreprocessFuncCall2

                        -- (E) check if user is invoking tag constructor prefix
                        V.TypeTagConstructorPrefix taggedUnionName tags _ scope ->
                            if length funcIds == 1 then
                                let tagname = head funcIds in
                                case find (\t -> snd (V.tagnameOf t) == snd tagname) tags of
                                    -- if is carryless tag
                                    Just (V.CarrylessTag tag (V.TaggedUnion name ids _ innerTypes)) -> 
                                        let (ctx3, subst) = instantiateTypeVar ctx innerTypes in
                                        let resultingInnerTypes = map (applySubstitutionToType subst) innerTypes in
                                        let belongingUnion = V.TaggedUnion name ids tags resultingInnerTypes in
                                        (Right (ctx3, First (V.Expr 
                                            (V.CarrylessTagExpr tag tagname scope) 
                                            ((V.TypeTaggedUnion belongingUnion)))))

                                    -- if is carryful tag
                                    Just (V.CarryfulTag _ expectedCarryType (V.TaggedUnion name ids tags innerTypes)) ->
                                        if length params' == 2 then do
                                            let carryExpr = params' !! 1 
                                            -- instantiate type vars
                                            let (ctx3, subst1) = instantiateTypeVar ctx2 innerTypes 
                                            let expectedCarryType' = applySubstitutionToType subst1 expectedCarryType

                                            -- verify carry expr
                                            (ctx4, verifiedCarryExpr) <- verifyExpr ctx3 assumption carryExpr
                                            subst2 <- unify verifiedCarryExpr expectedCarryType'

                                            let innerTypes' = map (applySubstitutionToType subst1) innerTypes
                                            let resultingInnerTypes = map (applySubstitutionToType subst2) innerTypes'
                                            Right (ctx4, First (V.Expr 
                                                (V.CarryfulTagExpr tagname verifiedCarryExpr scope)
                                                (V.TypeTaggedUnion (V.TaggedUnion name ids tags resultingInnerTypes))))
                                        else 
                                            Left (KErrorIncorrectUsageOfTagConstructorPrefix expression)


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
                (ctx3, result) <- lookupFunction ctx2 assumption params funcIds
                Right (ctx3, First result)

    Raw.Lambda param body _ ->
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

-- NOTE: params should be type checked using typeCheckExprs before passing into the lookupFunction function
lookupFunction 
    :: Context 
    -> Assumption 
    -> [V.Expr] 
    -> [Raw.StringToken] 
    -> Either KeliError (Context, V.Expr)

lookupFunction ctx@(Context _ env importedEnvs) assumption funcCallParams funcIds = do
    -- lookup the function being invoked from all the imported modules
    let lookupResult = lookupEnvs (intercalate "$" (map snd funcIds)) env importedEnvs
    let results = map (lookupFunction' ctx assumption funcCallParams funcIds) lookupResult
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
                    case find (\e -> case e of KErrorPartiallyMatchedFuncFound{}->True; _->False) errors of
                        Just e ->
                            Left e

                        _ ->
                            Left (KErrorUsingUndefinedFunc funcIds [])


        -- if only ONE matching function is found
        x:[] ->
            Right x

        -- if more than one matching functions are found
        _ ->
            Left (KErrorAmbiguousUsage funcIds lookupResult)




-- this is a helper function to lookup for matching function within a SINGLE module only 
lookupFunction'
    :: Context 
    -> Assumption 
    -> [V.Expr] 
    -> [Raw.StringToken] 
    -> (V.Scope, KeliSymbol)
    -> Either KeliError (Context, V.Expr)

lookupFunction' ctx@(Context _ env importedEnvs) assumption funcCallParams funcIds lookupResult = 
    case lookupResult of
        (scope, KeliSymFunc candidateFuncs) -> do
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
                                                                updatedEnv <- insertSymbolIntoEnv (KeliSymLocalConst paramName inputType') env 
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
                                                                let funcCall = V.Expr (V.FuncCall funcCallParams' funcIds (scope,currentFunc)) (expectedReturnType') in
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
                    Left (KErrorPartiallyMatchedFuncFound err)

                StillNoMatchingFunc ->
                    Left (KErrorUsingUndefinedFunc funcIds candidateFuncs)
        
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
                                        
lookupEnvs :: String -> Env -> [(ModuleName,Env)] -> [(V.Scope,KeliSymbol)]
lookupEnvs identifier currentEnv importedEnvs = 
    case lookup identifier currentEnv of
        Just s ->
            [(V.FromCurrentScope, s)]
        
        Nothing ->
            let result = 
                        map 
                            (\(modulename, env) ->
                                case lookup identifier env of
                                    Just s ->
                                        Just (V.FromImports modulename, s)
                                    Nothing ->
                                        Nothing) 
                            importedEnvs in

            let symbols = catMaybes result in
            symbols