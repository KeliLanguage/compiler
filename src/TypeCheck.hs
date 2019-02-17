{-# LANGUAGE BangPatterns #-}
module TypeCheck where


import Control.Monad
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

typeCheckExpr :: Context -> Assumption -> Raw.Expr -> Either KeliError (Context, OneOf3 V.Expr V.Type [V.UnlinkedTag])
typeCheckExpr ctx@(Context nextInt env) assumption expression = case expression of 
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

    Raw.Id token@(_,id) -> 
        case lookup id env of 
            Just (KeliSymConst _ expr) -> 
                Right (ctx, First (V.Expr (V.Id token) (getType expr)))
        
            Just (KeliSymType t@(V.TypeRecord name propTypePairs)) -> 
                -- Question: How are we gonna know if user is using this as type annotation or as record constructor?
                -- Answer: Using assumptions

                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (ctx, Second t))
                    CanBeAnything ->
                        (Right (ctx, First (V.Expr (V.RecordConstructor propTypePairs) ((V.TypeRecordConstructor name propTypePairs)))))

            Just (KeliSymType t@((V.TypeTaggedUnion (V.TaggedUnion name ids tags innerTypes)))) ->
                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (ctx, Second t))
                    CanBeAnything ->
                        (Right (ctx, First (V.Expr (V.TagConstructorPrefix) ( (V.TypeTagConstructorPrefix name tags innerTypes)))))
            
            Just (KeliSymType t) ->
                (Right (ctx, Second t))
            
            Just (KeliSymTypeConstructor t@(V.TaggedUnion name _ tags typeParams)) ->
                case assumption of
                    StrictlyAnalyzingType ->
                        Right (ctx, First (V.Expr (V.TypeConstructorPrefix) ( (V.TypeTypeConstructor t))))

                    CanBeAnything -> 
                        (Right (ctx, First (V.Expr (V.TagConstructorPrefix) ( (V.TypeTagConstructorPrefix name tags typeParams)))))

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
                    case find (\(_,x) -> x /= "#") funcIds of
                        Just x ->
                            Left (KErrorExpectedHashTag x)
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
                                                            (ctx2, keyTypePairs) <- verifyKeyTypePairs ctx keys (tail params'')
                                                            Right (ctx2, prevTags ++ [V.UnlinkedCarryfulTag tagname keyTypePairs])

                                                        other ->
                                                            Left (KErrorExpectedId other)
                                            _ ->
                                                Left (KErrorExpectedFuncCallOrId currentExpr)) 
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
                        (ctx2, firstValue) <- typeCheckExpr ctx CanBeAnything (tail params' !! 0)  -- <-- this line

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
                                (ctx3, keyTypePairs) <- verifyKeyTypePairs ctx keys (tail params')
                                Right (ctx3, Second ( (V.TypeRecord Nothing keyTypePairs)))

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
            continuePreprocessFuncCall1 :: Either KeliError (Context, OneOf3 V.Expr V.Type [V.UnlinkedTag])
            continuePreprocessFuncCall1 = do
                (ctx2, firstParam) <- verifyExpr ctx assumption (head params')

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

                        -- (B) check if user is calling tag matchers
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
                                    (ctx2, typeCheckedTagBranches) <- 
                                            foldM 
                                                (\(prevCtx, typeCheckedTagBranches') nextRawTagBranch -> do
                                                    (nextCtx, typeCheckedTagBranch) <- typeCheckTagBranch prevCtx expectedTags nextRawTagBranch
                                                    Right (nextCtx, typeCheckedTagBranches' ++ [typeCheckedTagBranch]))
                                                (ctx, [])
                                                rawTagBranches

                                    -- 2.2 type check raw else branches
                                    (ctx3, typeCheckedElseBranches) <- verifyExprs ctx2 assumption (map snd rawElseBranches)

                                    -- 3. check if each branch have the same type with the first branch
                                    case allBranchTypeAreSame (typeCheckedTagBranches ++ map (V.ElseBranch) typeCheckedElseBranches) of
                                        Left err ->
                                            Left err
                                            -- Left (KErrorNotAllBranchHaveTheSameType branches)

                                        Right branchType ->
                                            -- 4. check for exhaustiveness
                                            let actualTagnames = map (\b -> case b of 
                                                    V.CarrylessTagBranch (V.VerifiedTagname name) _ ->
                                                        name
                                                    V.CarryfulTagBranch (V.VerifiedTagname name) _ _ ->
                                                        name
                                                    ) typeCheckedTagBranches in
                                            
                                            let expectedTagnames = map (V.tagnameOf) expectedTags in

                                            case match actualTagnames expectedTagnames of
                                                PerfectMatch ->
                                                    Right (ctx2, First (V.Expr 
                                                        (V.TagMatcher subject typeCheckedTagBranches Nothing) 
                                                        branchType))

                                                GotDuplicates duplicates ->
                                                    Left (KErrorDuplicatedTags duplicates)

                                                Missing tags ->
                                                    case length typeCheckedElseBranches of
                                                        0 ->
                                                            Left (KErrorMissingTags subject tags)

                                                        1 ->
                                                            let elseBranch = head typeCheckedElseBranches in
                                                            Right (ctx2, First (V.Expr 
                                                                (V.TagMatcher subject typeCheckedTagBranches (Just elseBranch)) 
                                                                branchType))

                                                        _ ->
                                                            Left (KErrorMoreThanOneElseBranch (map fst rawElseBranches))

                                                other ->
                                                    error (show other)
                                                    -- error "Shoudln't reach here, because `typeCheckTagBranch` already check for unknown tags"


                        -- (C) check if user is calling record getter/setter
                        recordType@(V.TypeRecord name kvs) ->  
                            if length funcIds > 1 then
                                continuePreprocessFuncCall2
                            else
                                let subject = firstParam in
                                case find (\((_,key),_) -> key == snd (funcIds !! 0)) kvs of
                                    Just (propertyName, expectedType) -> 
                                        -- Check if is getter or setter
                                        if length (tail params') == 0 then -- is getter
                                            Right (ctx, First (V.Expr (V.RecordGetter subject propertyName) expectedType))
                                        else if length (tail params') == 1 then do -- is setter
                                            (ctx2, newValue) <- verifyExpr ctx assumption ((tail params') !! 0)
                                            substitution <- unify newValue expectedType
                                            Right (ctx2, First (V.Expr 
                                                (V.RecordSetter subject propertyName newValue) 
                                                (applySubstitutionToType substitution recordType)))
                                        else
                                            continuePreprocessFuncCall2

                                    Nothing -> 
                                        continuePreprocessFuncCall2

                        -- (D) check if user is invoking tag constructor prefix
                        V.TypeTagConstructorPrefix taggedUnionName tags _ ->
                            if length funcIds == 1 then
                                let tagname = head funcIds in
                                case find (\t -> snd (V.tagnameOf t) == snd tagname) tags of
                                    -- if is carryless tag
                                    Just (V.CarrylessTag tag (V.TaggedUnion name ids tags innerTypes)) -> 
                                        let (ctx2, subst) = instantiateTypeVar ctx innerTypes in
                                        let resultingInnerTypes = map (applySubstitutionToType subst) innerTypes in
                                        let belongingUnion = V.TaggedUnion name ids tags resultingInnerTypes in
                                        (Right (ctx2, First (V.Expr 
                                            (V.CarrylessTagExpr tag tagname) 
                                            ((V.TypeTaggedUnion belongingUnion)))))

                                    -- if is carryful tag
                                    Just (V.CarryfulTag tagname expectedPropTypePairs belongingUnion@(V.TaggedUnion name ids _ typeParams)) ->
                                        (Right (ctx, First (V.Expr
                                            (V.CarryfulTagConstructor tagname expectedPropTypePairs)
                                            ((V.TypeCarryfulTagConstructor 
                                                tagname
                                                expectedPropTypePairs
                                                belongingUnion)))))
                                    Nothing ->
                                        Left (KErrorTagNotFound tagname taggedUnionName tags)

                            else
                                Left (KErrorIncorrectUsageOfTagConstructorPrefix expression)

                        -- (E) check if user is invoking type constructor
                        V.TypeTypeConstructor t@(V.TaggedUnion name ids tags expectedTypeParams) ->
                            if map snd funcIds /= map snd ids then
                                Left (KErrorTypeConstructorIdsMismatch funcIds)
                            else do
                                -- TODO: check if type param conforms to type constraint
                                (ctx3, types) <- verifyTypes ctx (tail params')
                                Right (ctx3, Second ( (V.TypeTaggedUnion t)))

                        -- (F) check if user is invoking carryful tag constructor
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
                                    Right (ctx3, First (V.Expr expr' castType'))

                                _ ->
                                    undefined
                        else
                            treatAsNormalFuncCall
                    
                    -- otherwise
                    _ -> 
                        treatAsNormalFuncCall

                        
            treatAsNormalFuncCall = do
                (ctx2, params) <- verifyExprs ctx assumption params'
                (ctx3, result) <- typeCheckFuncCall ctx2 params funcIds
                Right (ctx3, First result)

    other -> 
        undefined


verifyType :: Context -> Raw.Expr -> Either KeliError (Context, V.Type) 
verifyType ctx rawExpr = do
    (ctx2, expr) <- typeCheckExpr ctx StrictlyAnalyzingType rawExpr
    type' <- extractType expr 
    Right (ctx2, type')

verifyTypes :: Context -> [Raw.Expr] -> Either KeliError (Context, [V.Type]) 
verifyTypes ctx rawExprs = do
    foldM 
        (\(prevCtx, verifiedTypes) nextRawExpr -> do
            (nextCtx, verifiedType) <- verifyType prevCtx nextRawExpr
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
        V.Func     -- matching function

    | PartiallyMatchedFuncFound -- means the 2nd params onward does not match expected types
        KeliError  -- corresponding error (should be type mismatch error)

    | StillNoMatchingFunc

-- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
typeCheckFuncCall :: Context -> [V.Expr] -> [Raw.StringToken] -> Either KeliError (Context, V.Expr)
typeCheckFuncCall ctx@(Context _ env) funcCallParams funcIds = 
    let funcId = intercalate "$" (map snd funcIds) in
    case lookup funcId env of
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
                                let expectedParamTypes = map (\(_,paramType) -> applySubstitutionToType subst1 paramType) (V.funcDeclParams currentFunc) 

                                -- (C) check if the firstParam matches the firstParamExpectedType
                                case unify (head funcCallParams) (head expectedParamTypes) of
                                    -- if matches, continue to unify the following params
                                    Right subst2 -> 
                                        case foldM 
                                            (\prevSubst (actualParam, expectedParamType) -> do
                                                -- apply previous substituion to current expectedParamType
                                                let expectedParamType' = applySubstitutionToType prevSubst expectedParamType
                                                nextSubst <- unify actualParam expectedParamType'
                                                Right (composeSubst prevSubst nextSubst))
                                            subst2
                                            (zip (tail funcCallParams) (tail expectedParamTypes)) of

                                            Left err ->
                                                PartiallyMatchedFuncFound err

                                            Right subst3 ->
                                                let expectedReturnType = applySubstitutionToType subst1 (V.funcDeclReturnType currentFunc) in
                                                let expectedReturnType' = applySubstitutionToType (composeSubst subst2 subst3) expectedReturnType in
                                                PerfectlyMatchedFuncFound ctx2 currentFunc{V.funcDeclReturnType = expectedReturnType'}

                                    -- if not, 
                                    Left _ ->
                                        StillNoMatchingFunc)

                    
                        StillNoMatchingFunc

                        -- This sorting is necessary so that the compiler will look for more specialized (a.k.a less generic) function first
                        (sortOn (\f -> length (V.funcDeclGenericParams f)) candidateFuncs)) of

                PerfectlyMatchedFuncFound newCtx f ->
                    let funcCall = V.FuncCall funcCallParams funcIds f in
                    Right (newCtx, V.Expr funcCall (V.funcDeclReturnType f))

                PartiallyMatchedFuncFound err ->
                    Left err

                StillNoMatchingFunc ->
                    Left (KErrorUsingUndefinedFunc funcIds candidateFuncs)
        
        Just other ->
            Left (KErrorNotAFunction funcIds)

        _ -> 
            Left (KErrorUsingUndefinedFunc funcIds [])

verifyBoundedTypeVar 
    :: Context 
    -> (Raw.StringToken, Raw.Expr) 
    -> Either KeliError (V.StringToken, Maybe V.TypeConstraint)
verifyBoundedTypeVar ctx (name, expr) = do
    (_, result) <- verifyType ctx expr
    case result of
        V.TypeType ->
            Right (name, Nothing)

        _ ->
            Left (KErrorInvalidTypeParamDecl expr)


hardConformsTo :: V.Type -> (Maybe V.TypeConstraint) -> Bool
type' `hardConformsTo` Nothing = True
type' `hardConformsTo` (Just constraint) = 
    case constraint of
        V.ConstraintAny -> 
            True
        _ -> 
            undefined

extractTag :: OneOf3 V.Expr V.Type [V.UnlinkedTag] -> Either KeliError [V.UnlinkedTag]
extractTag x =
    case x of
        First expr   -> Left (KErrorExpectedTagButGotExpr expr)
        Second type' -> Left (KErrorExpectedTagButGotType type')
        Third tags   -> Right tags

extractType :: OneOf3 V.Expr V.Type [V.UnlinkedTag] -> Either KeliError V.Type
extractType x = 
    case x of
        First expr   -> Left (KErrorExpectedTypeButGotExpr expr)
        Second type' -> Right type' 
        Third tag    -> Left (KErrorExpectedTypeButGotTag tag)



extractExpr :: OneOf3 V.Expr V.Type [V.UnlinkedTag] -> Either KeliError V.Expr
extractExpr x = 
    case x of 
        First expr -> Right expr
        Second type' -> Left (KErrorExpectedExprButGotType type')
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

insertSymbolIntoSymtab :: KeliSymbol -> Env -> Either KeliError Env
insertSymbolIntoSymtab symbol env =
    case symbol of 
        KeliSymFunc [f] -> 
            let funcid = (intercalate "$" (map snd (V.funcDeclIds f))) in
            let funcsWithSameName = lookup funcid env in
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
                    Right (env |> (funcid, KeliSymFunc (f:fs)))
                
                Just _ ->
                    Left (KErrorDuplicatedId (V.funcDeclIds f))

                Nothing ->
                    Right (env |> (funcid, symbol))

        KeliSymInlineExprs exprs ->
            case lookup "@inline_exprs" env of
                Just (KeliSymInlineExprs exprs') ->
                    Right (env |> ("@inline_exprs", KeliSymInlineExprs (exprs' ++ exprs)))

                Just _ ->
                    error "shouldn't reach here"
                
                Nothing ->
                    Right (env |> ("@inline_exprs", KeliSymInlineExprs exprs))

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

        KeliSymTypeConstructor (V.TaggedUnion name _ _ _) ->
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
            Right (ctx, zip keys typeCheckedExprs)

verifyKeyTypePairs 
    :: Context 
    -> [Raw.StringToken] 
    -> [Raw.Expr] 
    -> Either KeliError (Context, [(V.StringToken, V.Type)])
verifyKeyTypePairs ctx keys types = do
    case findDuplicates keys of
        Just duplicates ->
            Left (KErrorDuplicatedProperties duplicates)
        Nothing -> do
            (ctx2, verifiedTypes) <- verifyTypes ctx types
            Right (ctx2, zip keys verifiedTypes)


-- copied from https://stackoverflow.com/questions/49843681/getting-even-and-odd-position-of-elements-in-list-haskell-mutual-recursion/49844021
-- Retrieve even-indexed elements 
evens (x:xs) = x:odds xs
evens _ = []

-- Retrieve odd-indexed elements
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
createNewTVar (Context nextInt env) constraint =
    (Context (nextInt + 1) env, V.FreeTypeVar ("T$" ++ show nextInt) constraint)


typeCheckTagBranch 
    :: Context 
    -> [V.Tag]
    -> (Raw.Expr, Raw.Expr) 
    -> Either KeliError (Context, V.TagBranch)
typeCheckTagBranch ctx@(Context _ env) expectedTags (tag, branch) =
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
                                        insertSymbolIntoSymtab (KeliSymConst bindingId (V.Expr (V.Id bindingId) expectedType)) prevSymtab)
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
                                        