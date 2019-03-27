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
import Unify

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
                        !_ <- forM tailElements (\element -> unify element typeOfFirstElem)
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
        substitution <- unify actualObject (V.TypeObject aliasedName expectedPropTypePairs)
        let resultingType = applySubstitutionToType substitution (V.TypeObject aliasedName expectedPropTypePairs)
        Right (ctx3, First (V.Expr (V.Object actualPropValuePairs) resultingType))

-- (B) check if user is performing function application
continuePreprocessFuncCall1 ctx assumption 
    func@(V.Expr _ (V.TypeTaggedUnion (V.TaggedUnion (_,"Function") _ _ (expectedInputType:expectedOutputType:[]))))
    (appliedValue:[]) 
    ((_,"apply"):[]) = do
        (ctx3, value) <- verifyExpr ctx assumption appliedValue
        -- check if value match with expectedInputType
        !_ <- unify value expectedInputType
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
        !_ <- mapM (\(pos,x) -> if x == "then"  then Right() else Left (KErrorExpectedColon (pos,x))) (odds elselessFuncIds)

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
        case allBranchTypeAreSame (typeCheckedTagBranches ++ map (V.ElseBranch) typeCheckedElseBranches) of
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

                                subst2 <- unify (verifiedLambdaBody) (expectedType)
                                let returnType = applySubstitutionToType subst2 objectType
                                Right (ctx4, First (V.Expr 
                                    (V.ObjectLambdaSetter 
                                        subject
                                        propertyName
                                        lambdaParam
                                        verifiedLambdaBody)
                                    (returnType)))

                            -- else is value setter
                            _ -> do
                                (ctx3, newValue) <- verifyExpr ctx assumption secondParam
                                substitution <- unify newValue expectedType
                                let returnType = applySubstitutionToType substitution objectType
                                Right (ctx3, First (V.Expr 
                                    (V.ObjectSetter subject propertyName newValue) 
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
                    (V.CarrylessTagExpr tag tagname scope) 
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
                        subst2 <- unify verifiedCarryExpr expectedCarryType'

                        let resultingTaggedUnionType = 
                                applySubstitutionToType subst2 
                                    (applySubstitutionToType subst1 
                                        (V.TypeTaggedUnion taggedUnion))

                        -- update expected carry type of every carryful tag

                        Right (ctx4, First (V.Expr 
                            (V.CarryfulTagExpr tagname verifiedCarryExpr scope)
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
    -> Assumption 
    -> [V.Expr]
    -> [Raw.StringToken]
    -> (V.Scope, KeliSymbol)
    -> Either KeliError (Context, V.Expr, V.FuncSignature)

lookupFunction' ctx@(Context _ env importedEnvs) assumption (firstParam:tailParams) funcIds lookupResult = 
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
                                let expectedParamTypes@(firstExpectedParamType:tailExpectedParamTypes) = 
                                        map (\(_,paramTypeAnnot) -> 
                                            applySubstitutionToType subst1 (V.getTypeRef paramTypeAnnot)) (V.funcDeclParams currentFunc) 

                                -- (C) check if the firstParam matches the firstParamExpectedType
                                case unify firstParam firstExpectedParamType of
                                    -- if matches, continue to unify the following params
                                    Right subst2 -> 
                                        case unifyMany subst2 tailParams tailExpectedParamTypes of
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
                                                                Right e) (firstParam:tailParams) of
                                                    Right funcCallParams' -> 
                                                        let expectedReturnType = applySubstitutionToType subst1 (V.funcDeclReturnType currentFunc) in
                                                        -- unify again to get the best substitution (mainly for inferring lambdas)
                                                        case unifyMany subst4 funcCallParams' expectedParamTypes of
                                                            Left err ->
                                                                PartiallyMatchedFuncFound err
                                                            
                                                            Right subst5 ->
                                                                let expectedReturnType' = applySubstitutionToType subst5 expectedReturnType in
                                                                let funcCall = V.Expr (V.FuncCall funcCallParams' funcIds (scope,currentFunc)) (expectedReturnType') in
                                                                PerfectlyMatchedFuncFound ctx2 funcCall currentFunc
                                                    
                                                    Left err ->
                                                        PartiallyMatchedFuncFound err


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


lookupFunction' ctx@(Context _ env importedEnvs) assumption [] funcIds lookupResult = error "impossible"

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
    :: [V.TagBranch]
    -> Either 
        KeliError 
        V.Type -- type of the first branch
allBranchTypeAreSame typeCheckedTagBranches = do
    let (firstBranch:tailBranches) = map (\b -> case b of 
            V.CarryfulTagBranch _ _ expr -> expr
            V.CarrylessTagBranch _ expr -> expr
            V.ElseBranch expr -> expr) typeCheckedTagBranches

    let expectedTypeOfEachBranches = getType firstBranch 
    case mapM (\b -> unify b expectedTypeOfEachBranches) tailBranches of
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