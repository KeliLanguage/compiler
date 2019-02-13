{-# LANGUAGE BangPatterns #-}
module TypeCheck where


import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered ((|>), lookup, member) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError
import Symbol
import Util

getType :: V.Expr -> V.Type
getType (V.Expr _ type') = type'

data Assumption 
    = StrictlyAnalyzingType
    | CanBeAnything 

typeCheckExpr :: KeliSymTab -> Assumption -> Raw.Expr -> Either KeliError (OneOf3 V.Expr V.Type [V.UnlinkedTag])
typeCheckExpr symtab assumption expression = case expression of 
    Raw.IncompleteFuncCall expr positionOfTheDotOperator -> do
        typeCheckedExpr <- typeCheckExpr symtab assumption expr 
        Left (KErrorIncompleteFuncCall typeCheckedExpr positionOfTheDotOperator)

    Raw.NumberExpr(pos,n) -> 
        case n of 
            Left intValue ->
                Right (First (V.Expr (V.IntExpr (pos, intValue)) (V.ConcreteType V.TypeInt)))

            Right doubleValue ->
                Right (First (V.Expr (V.DoubleExpr (pos, doubleValue)) (V.ConcreteType V.TypeFloat)))

    Raw.StringExpr (pos, str) -> 
        Right (First (V.Expr (V.StringExpr (pos, str)) (V.ConcreteType V.TypeString)))

    Raw.Id token@(_,id) -> 
        case lookup id symtab of 
            Just (KeliSymConst _ expr) -> 
                Right (First (V.Expr (V.Id token) (getType expr)))
        
            Just (KeliSymType (V.TypeAlias _ t@(V.ConcreteType (V.TypeRecord propTypePairs)))) -> 
                -- Question: How are we gonna know if user is using this as type annotation or as record constructor?
                -- Answer: Using assumptions

                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (Second t))
                    CanBeAnything ->
                        (Right (First (V.Expr (V.RecordConstructor propTypePairs) (V.ConcreteType(V.TypeRecordConstructor propTypePairs)))))

            Just (KeliSymType (V.TypeAlias _ t@(V.ConcreteType(V.TypeTaggedUnion (V.TaggedUnion name ids tags typeParams))))) ->
                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (Second t))
                    CanBeAnything ->
                        (Right (First (V.Expr (V.TagConstructorPrefix) (V.ConcreteType (V.TypeTagConstructorPrefix name tags Nothing)))))
            
            Just (KeliSymType (V.TypeAlias _ t)) ->
                (Right (Second t))
            
            Just (KeliSymType (V.TypeAlias _ (V.TypeVariable name constraint isRigid))) ->
                Right (Second (V.TypeVariable name constraint isRigid))

            Just (KeliSymTypeConstructor t@(V.TaggedUnion name _ tags typeParams)) ->
                case assumption of
                    StrictlyAnalyzingType ->
                        Right (First (V.Expr (V.TypeConstructorPrefix) (V.ConcreteType (V.TypeTypeConstructor t))))

                    CanBeAnything -> 
                        (Right (First (V.Expr (V.TagConstructorPrefix) (V.ConcreteType (V.TypeTagConstructorPrefix name tags typeParams)))))

            Nothing -> 
                Left (KErrorUsingUndefinedId token)
            
            other -> 
                error (show other)
                undefined

    Raw.FuncCall params' funcIds -> do
        case head funcIds of 
            -- 0. Check if user wants to create a tagged union
            (_,"or") -> do
                let isTagOrUnion x = 
                        case x of 
                            Second (V.ConcreteType (V.TypeTaggedUnion{})) -> True; 
                            Third _ -> True; 
                            _ -> False;
                params <- mapM (typeCheckExpr symtab assumption) params';
                if isTagOrUnion (head params) then do
                    tags <- mapM extractTag params
                    Right (Third (concat tags))
                else  do
                    continuePreprocessFuncCall1

            _ -> do  
                case (head params') of
                    (Raw.Id firstParamToken@(_,firstParamId)) -> 
                        case firstParamId of
                        -- 1. Check if user wants to create a tagged union
                        "tags" ->
                            case find (\(_,x) -> x /= "#") funcIds of
                                Just x ->
                                    Left (KErrorExpectedHashTag x)
                                Nothing -> do
                                    tags <- mapM 
                                            (\e -> 
                                                case e of
                                                    Raw.Id id ->
                                                        Right (V.UnlinkedCarrylessTag id)
                                                    Raw.FuncCall params'' funcIds'' ->
                                                        if length params'' < 2 then
                                                            Left (KErrorExpectedTypeAnnotationAfterThis (last funcIds''))
                                                        else do
                                                            case head params'' of
                                                                Raw.Id tagname -> do
                                                                    let keys = funcIds''
                                                                    keyTypePairs <- verifyKeyTypePairs symtab keys (tail params'')
                                                                    Right (V.UnlinkedCarryfulTag tagname keyTypePairs)

                                                                other ->
                                                                    Left (KErrorExpectedId other)
                                                    _ ->
                                                        Left (KErrorExpectedFuncCallOrId e)
                                                ) (tail params')
                                    Right (Third tags)

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
                                firstValue <- typeCheckExpr symtab CanBeAnything (tail params' !! 0)  -- <-- this line

                                let keys = funcIds
                                case firstValue of 
                                    -- assume user want to create a record value
                                    First _ -> do
                                        keyValuePairs <- verifyKeyValuePairs symtab keys (tail params')
                                        Right 
                                            (First
                                                (V.Expr
                                                    (V.Record keyValuePairs) 
                                                    (V.ConcreteType (V.TypeRecord (map (\(k, expr) -> (k, getType expr)) keyValuePairs)))))
                                    
                                    -- assume user want to declare a record type
                                    Second _ -> do
                                        keyTypePairs <- verifyKeyTypePairs symtab keys (tail params')
                                        Right (Second (V.ConcreteType (V.TypeRecord keyTypePairs)))

                                    Third tag -> 
                                        Left (KErrorExpectedExprOrTypeButGotTag tag)

                        -- 3. check if user is using javascript ffi
                        "ffi" ->
                            if length funcIds /= 1 || length params' /= 2 then
                                Left (KErrorIncorrectUsageOfFFI firstParamToken)
                            else if snd (funcIds !! 0) /= "javascript" then
                                Left (KErrorUnknownFFITarget (funcIds !! 0))
                            else do
                                jsCode <- typeCheckExpr symtab assumption (params' !! 1) >>= extractExpr
                                case jsCode of
                                    (V.Expr (V.StringExpr value) _) ->
                                        Right (First (V.Expr (V.FFIJavascript value) (V.ConcreteType V.TypeUndefined)))

                                    _ -> 
                                        Left (KErrorFFIValueShouldBeString jsCode)
                            
                        _ -> 
                            continuePreprocessFuncCall1

                    _ -> 
                        continuePreprocessFuncCall1

        where 
            continuePreprocessFuncCall1 :: Either KeliError (OneOf3 V.Expr V.Type [V.UnlinkedTag])
            continuePreprocessFuncCall1 = do
                firstParam <- typeCheckExpr symtab assumption (head params') >>= extractExpr

                let typeOfFirstParam = getType firstParam in
                    case typeOfFirstParam of
                        V.TypeVariable _ _ _ ->
                            undefined

                        V.ConcreteType typeOfFirstParam' ->
                            -- (A) check if user is invoking record constructor
                            case typeOfFirstParam' of
                            V.TypeRecordConstructor expectedPropTypePairs -> do
                                let expectedProps = map fst expectedPropTypePairs
                                let actualProps = funcIds 
                                case match actualProps expectedProps of
                                    GotDuplicates duplicates ->
                                        Left (KErrorDuplicatedProperties duplicates)

                                    ZeroIntersection ->
                                        continuePreprocessFuncCall2
                                    
                                    GotExcessive excessiveProps ->
                                        Left (KErrorExcessiveProperties excessiveProps)
                                    
                                    Missing missingProps ->
                                        Left (KErrorMissingProperties firstParam missingProps)
                                    
                                    PerfectMatch ->  do
                                        values  <- typeCheckExprs symtab assumption (tail params') >>= mapM extractExpr 

                                        let verifiedPropValuePairs = zip funcIds values

                                        let expectedPropTypePairs' = sortBy (\((_,a),_) ((_,b),_) -> compare a b) expectedPropTypePairs 
                                        let actualPropValuePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) verifiedPropValuePairs 
                                        case foldl' 
                                            -- reducer
                                            (\prevResult (expected, actual) -> 
                                                case prevResult of
                                                    Right updatedSymtab ->
                                                        let expectedType = snd expected in
                                                        case typeCompares symtab (snd actual) expectedType of
                                                            ApplicableOk _  ->
                                                                Right updatedSymtab
                                                            
                                                            ApplicableFailed err ->
                                                                Left err
                                                                -- Left (KErrorPropertyTypeMismatch (fst expected) (snd expected) (snd actual))

                                                            NotApplicable newSymtab ->
                                                                Right newSymtab

                                                    Left err ->
                                                        Left err) 

                                            -- initial value
                                            (Right symtab)

                                            --foldee
                                            (zip expectedPropTypePairs' actualPropValuePairs) of

                                                Right _ ->
                                                    Right (First (V.Expr (V.Record actualPropValuePairs) (V.ConcreteType (V.TypeRecord expectedPropTypePairs'))))
                                                
                                                Left err ->
                                                    Left err

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
                                        typeCheckedTagBranches <- mapM typeCheckTagBranch rawTagBranches

                                        -- 2.2 type check raw else branches
                                        typeCheckedElseBranches <- typeCheckExprs symtab assumption (map snd rawElseBranches) >>= mapM extractExpr

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
                                                        Right (First (V.Expr 
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
                                                                Right (First (V.Expr 
                                                                    (V.TagMatcher subject typeCheckedTagBranches (Just elseBranch)) 
                                                                    branchType))

                                                            _ ->
                                                                Left (KErrorMoreThanOneElseBranch (map fst rawElseBranches))

                                                    other ->
                                                        error (show other)
                                                        -- error "Shoudln't reach here, because `typeCheckTagBranch` already check for unknown tags"

                                where
                                    typeCheckTagBranch :: (Raw.Expr, Raw.Expr) -> Either KeliError V.TagBranch
                                    typeCheckTagBranch (tag, branch) =
                                        case tag of

                                            Raw.Id actualTagname -> do
                                                (verifiedTagname,_) <- verifyTagname expectedTags actualTagname
                                                typeCheckedBranch <- typeCheckExpr symtab assumption branch >>= extractExpr
                                                Right (V.CarrylessTagBranch verifiedTagname typeCheckedBranch)

                                            
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
                                                                
                                                                -- update symtab with property-bindings
                                                                updatedSymtab <- 
                                                                    foldM 
                                                                        (\prevSymtab (_,bindingId, expectedType) -> 
                                                                            insertSymbolIntoSymtab (KeliSymConst bindingId (V.Expr (V.Id bindingId) expectedType)) prevSymtab)
                                                                        symtab
                                                                        verifiedPropBindings

                                                                -- type check the branch
                                                                typeCheckedBranch <- typeCheckExpr updatedSymtab assumption branch >>= extractExpr

                                                                Right (V.CarryfulTagBranch verifiedTagname verifiedPropBindings typeCheckedBranch)

                                                    other ->
                                                        Left (KErrorExpectedId other)

                                                            
                                            other -> 
                                                Left (KErrorExpectedTagBindings other)


                                    allBranchTypeAreSame typeCheckedTagBranches = do
                                        let branches = map (\b -> case b of 
                                                V.CarryfulTagBranch _ _ expr -> expr
                                                V.CarrylessTagBranch _ expr -> expr
                                                V.ElseBranch expr -> expr) typeCheckedTagBranches

                                        let firstBranch = head branches 
                                        let expectedTypeOfEachBranches = getType firstBranch 
                                        (foldM 
                                            (\t branch -> 
                                                case typeCompares symtab branch expectedTypeOfEachBranches of
                                                    ApplicableFailed err ->
                                                        Left err

                                                    NotApplicable _ ->
                                                        Left (KErrorCannotMatchConcreteTypeWithRigidTypeVariable branch expectedTypeOfEachBranches)
                                                    
                                                    ApplicableOk _ ->
                                                        Right t) 

                                            expectedTypeOfEachBranches 
                                            branches) :: Either KeliError V.Type
                                            

                            -- (C) check if user is calling record getter/setter
                            recordType@(V.TypeRecord kvs) ->  
                                if length funcIds > 1 then
                                    continuePreprocessFuncCall2
                                else
                                    let subject = firstParam in
                                    case find (\((_,key),_) -> key == snd (funcIds !! 0)) kvs of
                                        Just (propertyName, expectedType) -> 
                                            -- Check if is getter or setter
                                            if length (tail params') == 0 then -- is getter
                                                Right (First (V.Expr (V.RecordGetter subject propertyName) expectedType))
                                            else if length (tail params') == 1 then do -- is setter
                                                newValue <- typeCheckExpr symtab assumption ((tail params') !! 0) >>= extractExpr
                                                case typeCompares symtab newValue expectedType of
                                                    ApplicableFailed err ->
                                                        Left err

                                                    _ ->
                                                        Right (First (V.Expr 
                                                            (V.RecordSetter subject propertyName newValue) (V.ConcreteType recordType)))
                                            else
                                                continuePreprocessFuncCall2

                                        Nothing -> 
                                            continuePreprocessFuncCall2

                            -- (D) check if user is invoking tag constructor prefix
                            V.TypeTagConstructorPrefix taggedUnionName tags _ ->
                                if length funcIds == 1 then
                                    let tagname = head funcIds in
                                    case find (\t -> snd (V.tagnameOf t) == snd tagname) tags of
                                        Just (V.CarrylessTag tag (V.TaggedUnion name ids _ typeParams)) -> 
                                            let resolvedTypeParams = 
                                                    (case typeParams of 
                                                        Just typeParams' -> Just (map derigidify typeParams'); 
                                                        _ -> typeParams) in

                                            let belongingUnion = V.TaggedUnion name ids tags resolvedTypeParams in

                                            (Right (First (V.Expr 
                                                (V.CarrylessTagConstructor tag tagname) 
                                                (V.ConcreteType (V.TypeTaggedUnion belongingUnion)))))

                                        Just (V.CarryfulTag tagname expectedPropTypePairs belongingUnion@(V.TaggedUnion name ids _ typeParams)) ->
                                            (Right (First (V.Expr
                                                (V.CarryfulTagConstructor tagname expectedPropTypePairs)
                                                (V.ConcreteType (V.TypeCarryfulTagConstructor 
                                                    tagname
                                                    expectedPropTypePairs
                                                    belongingUnion
                                                    typeParams)))))

                                            -- if length params' == 2 then do
                                            --     carryExpr  <- typeCheckExpr symtab assumption (params' !! 1) >>= extractExpr
                                            --     case typeCompares symtab carryExpr expectedCarryType of
                                            --         ApplicableFailed err ->
                                            --             Left err
                                            --             -- Left (KErrorIncorrectCarryType expectedCarryType carryExpr)

                                            --         ApplicableOk updatedSymtab ->
                                            --             -- TODO: reduce code duplication
                                            --             let x =
                                            --                     case typeParams of
                                            --                         Just typeParams' ->
                                            --                             let resolvedTypeParams = map (resolveType updatedSymtab) typeParams' in
                                            --                             Just resolvedTypeParams

                                            --                         Nothing ->
                                            --                             Nothing
                                            --             in
                                            --                 Right (First (V.Expr 
                                            --                     (V.CarryfulTagExpr tag carryExpr) (V.ConcreteType 
                                            --                     (V.TypeTaggedUnion (V.TaggedUnion name ids tags x)))))
                                                    
                                            --         NotApplicable updatedSymtab ->
                                            --             -- TODO: reduce code duplication
                                            --             let x =
                                            --                     case typeParams of
                                            --                         Just typeParams' ->
                                            --                             let resolvedTypeParams = map (resolveType updatedSymtab) typeParams' in
                                            --                             Just resolvedTypeParams

                                            --                         Nothing ->
                                            --                             Nothing
                                            --             in
                                            --                 Right (First (V.Expr 
                                            --                     (V.CarryfulTagExpr tag carryExpr) (V.ConcreteType 
                                            --                     (V.TypeTaggedUnion (V.TaggedUnion name ids tags x)))))

                                            -- else
                                            --     Left (KErrorExpectedACarry tagname)


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
                                    types <- typeCheckExprs symtab StrictlyAnalyzingType (tail params') >>= mapM extractType 
                                    Right (Second (V.ConcreteType (V.TypeTaggedUnion t)))

                            -- (F) check if user is invoking carryful tag constructor
                            V.TypeCarryfulTagConstructor tagname expectedPropTypePairs belongingUnion typeParams -> do
                                values <- typeCheckExprs symtab assumption (tail params') >>= mapM extractExpr
                                let actualPropValuePairs = zip funcIds values
                                case typeCompares symtab 
                                    (V.Expr (V.Record actualPropValuePairs) (V.ConcreteType (V.TypeRecord (map (\(k, v) -> (k, getType v)) actualPropValuePairs))))
                                    (V.ConcreteType (V.TypeRecord expectedPropTypePairs)) of

                                    ApplicableOk _ ->
                                        Right (First (V.Expr 
                                            (V.CarryfulTagExpr tagname actualPropValuePairs)
                                            (V.ConcreteType (V.TypeTaggedUnion belongingUnion))))
                                    
                                    ApplicableFailed err ->
                                        Left err
                                    
                                    NotApplicable updatedSymtab ->
                                        undefined

                            -- otherwise
                            _ ->
                                continuePreprocessFuncCall2

                    
            
            continuePreprocessFuncCall2 = 
                case funcIds !! 0 of
                    -- check if user is calling type casting
                    (_, "as") ->
                        if length funcIds == 1 && length params' == 2 then do
                            subject <- typeCheckExpr symtab assumption (params' !! 0) >>= extractExpr
                            castType <- typeCheckExpr symtab StrictlyAnalyzingType (params' !! 1) >>= extractType
                            case subject of
                                (V.Expr expr' (V.ConcreteType V.TypeUndefined)) ->
                                    Right (First (V.Expr expr' castType))

                                _ ->
                                    undefined
                        else
                            treatAsNormalFuncCall
                    
                    -- otherwise
                    _ -> 
                        treatAsNormalFuncCall

                        
            treatAsNormalFuncCall = do
                params <- typeCheckExprs symtab assumption params' >>= mapM extractExpr
                result <- typeCheckFuncCall symtab params funcIds
                Right (First result)

    other -> 
        undefined

data MatchFuncResult 
    = PerfectlyMatchedFuncFound 
        V.Func   -- matching function
        KeliSymTab -- corresponding environment

    | PartiallyMatchedFuncFound -- means the 2nd params onward does not match expected types
        KeliError  -- corresponding error (should be type mismatch error)

    | StillNoMatchingFunc

-- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
typeCheckFuncCall :: KeliSymTab -> [V.Expr] -> [Raw.StringToken] -> Either KeliError V.Expr
typeCheckFuncCall symtab funcCallParams funcIds = 
    let funcId = intercalate "$" (map snd funcIds) in
    case lookup funcId symtab of
        Just (KeliSymFunc candidateFuncs) -> do
            case (foldl 
                    (\result f ->
                        case result of
                        p@PerfectlyMatchedFuncFound{} -> p

                        p@PartiallyMatchedFuncFound{} -> p
                        
                        -- if
                        StillNoMatchingFunc -> -- then continue the loop for searching matching functions
                            if length funcCallParams /= length (V.funcDeclParams f) then
                                StillNoMatchingFunc
                            else do
                                let expectedParamTypes = map (\(_,paramType) -> paramType) (V.funcDeclParams f) 
                                let firstParam = head funcCallParams 
                                let fisrtParamExpectedType = head expectedParamTypes
                                -- if only have 1 param, then just check if the first param match the expected type
                                if length funcCallParams == 1 then 
                                    case typeCompares symtab firstParam fisrtParamExpectedType of
                                        ApplicableFailed {} ->
                                            StillNoMatchingFunc

                                        ApplicableOk _ ->
                                            PerfectlyMatchedFuncFound f symtab

                                        NotApplicable updatedSymtab ->
                                            PerfectlyMatchedFuncFound f updatedSymtab

                                -- if more than 1 param, then we need to check if every param match every expected types
                                -- why is this branching necessary? 
                                --     so that we can report a better error message
                                else 
                                    case typeCompares symtab firstParam fisrtParamExpectedType of
                                        -- if the first param does not match expected type
                                        ApplicableFailed {} ->
                                            StillNoMatchingFunc
                                        
                                        -- if the first param match the expected type, check all subsequent params
                                        _ -> 
                                            case foldM
                                                    (\tempSymtab (param, expectedType) -> 
                                                        case typeCompares tempSymtab param expectedType of
                                                            ApplicableFailed err -> 
                                                                Left err
                                                                -- Left (KErrorFuncCallTypeMismatch expectedType expr)
                                                            
                                                            ApplicableOk _ ->
                                                                Right tempSymtab
                                                            
                                                            NotApplicable updatedSymtab ->
                                                                Right updatedSymtab)
                                                    symtab
                                                    (zip funcCallParams expectedParamTypes) of 

                                                Right updatedSymtab ->
                                                    PerfectlyMatchedFuncFound f updatedSymtab

                                                Left err ->
                                                    PartiallyMatchedFuncFound err)
                    
                        StillNoMatchingFunc

                        -- This sorting is necessary so that the compiler will look for more specialized (a.k.a less generic) function first
                        (sortOn (\f -> length (V.funcDeclGenericParams f)) candidateFuncs)) of

                PerfectlyMatchedFuncFound f updatedSymtab ->
                    let funcCall = V.FuncCall funcCallParams funcIds f in
                    let funcCallReturnType = resolveType updatedSymtab (V.funcDeclReturnType f) in
                    Right (V.Expr funcCall funcCallReturnType)

                PartiallyMatchedFuncFound err ->
                    Left err

                StillNoMatchingFunc ->
                    Left (KErrorUsingUndefinedFunc funcIds candidateFuncs)
        
        Just other ->
            Left (KErrorNotAFunction funcIds)

        _ -> 
            Left (KErrorUsingUndefinedFunc funcIds [])

resolveType :: KeliSymTab -> V.Type -> V.Type
resolveType symtab t =
    case t of
        V.TypeVariable name _ isRigid ->
            case lookup (snd name) symtab of
                Just (KeliSymType (V.TypeAlias _ boundedType)) ->
                    boundedType

                Nothing ->
                    t

        V.ConcreteType (V.TypeTaggedUnion (V.TaggedUnion name ids tags (Just typeParams))) ->
            let resolvedTypeParams = map (resolveType symtab) typeParams in
            V.ConcreteType (V.TypeTaggedUnion (V.TaggedUnion name ids tags (Just resolvedTypeParams)))
        
        _ ->
            t

verifyType :: KeliSymTab -> Raw.Expr -> Either KeliError V.Type
verifyType symtab expr = typeCheckExpr symtab StrictlyAnalyzingType expr >>= extractType

verifyTypeParam :: KeliSymTab -> (Raw.StringToken, Raw.Expr) -> Either KeliError V.TypeParam
verifyTypeParam symtab (paramName, expr) = do
    result <- typeCheckExpr symtab StrictlyAnalyzingType expr
    case result of
        Second (V.ConcreteType V.TypeType) ->
            Right (V.TypeParam paramName Nothing)

        _ ->
            Left (KErrorInvalidTypeParamDecl expr)

typeCheckExprs :: KeliSymTab -> Assumption -> [Raw.Expr] -> Either KeliError [OneOf3 V.Expr V.Type [V.UnlinkedTag]]
typeCheckExprs symtab assumption exprs = mapM (typeCheckExpr symtab assumption) exprs


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

data TypeCompareResult
    -- TODO: 2 cases should be sufficient
    {- 
    NOTE:
        Applicable means 
            both operands are not type parameters OR both operands are type parameters (with the same name)

        NotApplicable means
            one of the operand is a type parameter
    -}
    = ApplicableOk -- ok means actual type equals to expected type
        KeliSymTab -- this is only needed for type checking record types
    
    | ApplicableFailed -- failed means actual type does not equals expected type
        KeliError

    | NotApplicable
        KeliSymTab  --  an updated environment, which binds the type parameter to the comparer type

instance Show TypeCompareResult where
    show ApplicableOk{} = "ApplicableOk"
    show ApplicableFailed{} = "ApplicableFailed"
    show NotApplicable{} = "NotApplicable"

typeCompares :: 
    KeliSymTab 
    -> V.Expr -- actual expr
    -> V.Type -- expected type
    -> TypeCompareResult
typeCompares symtab (V.Expr expr actualType) expectedType =
    typeCompares' symtab (V.Expr expr actualType) (resolveType symtab expectedType)

typeCompares' :: 
    KeliSymTab 
    -> V.Expr -- actual expr
    -> V.Type -- expected type
    -> TypeCompareResult
typeCompares' symtab actualExpr@(V.Expr expr actualType) expectedType =
    case expectedType of 
        -- if expected type is a type variable, check if it is bounded
        V.TypeVariable name1 constraint1 isRigid1 ->
            case actualType of
                -- if both actualType and expectedType are type params, just compare their name
                typeVar2@(V.TypeVariable name2 constraint2 isRigid2) ->
                    if snd name1 == snd name2 then
                        ApplicableOk symtab
                    else
                        undefined
                    -- TODO: should I check if actualType is also bounded?

                -- if expectedType is a bounded type param AND actualType is a concrete type
                V.ConcreteType actualType' ->
                    -- lookup symtab to check if this type parameter is already bounded to a type 
                    case lookup (snd name1) symtab of
                        -- if bounded, compare actualType to boundedType
                        Just (KeliSymType (V.TypeAlias _ (V.ConcreteType boundedType))) ->
                            typeCompares'' symtab (expr, actualType') boundedType

                        -- if not bounded, update the environment (bound actualType to the type variable)
                        Nothing ->
                            NotApplicable (symtab |> (snd name1, KeliSymType (V.TypeAlias name1 actualType)))

                        other ->
                            error ("Actual type"   ++ show actualType' 
                            ++ ";\nActual expr:"   ++ show expr
                            ++ ";\nExpected type:" ++ show expectedType)

        V.ConcreteType expectedType' ->
            case actualType of
                -- if expectedType is concrete but actualType is type variable
                V.TypeVariable name constraint isRigid ->
                    ApplicableOk symtab
                    -- if isRigid then
                    --     error ("Actual type"   ++ show actualType
                    --     ++ ";\nActual expr:"   ++ show expr
                    --     ++ ";\nExpected type:" ++ show expectedType)
                    --     ApplicableFailed (KErrorCannotMatchRigidTypeVariableWithConcreteType
                    --         actualExpr
                    --         expectedType)
                    -- else

                -- if both are concrete types, just do direct comparison
                V.ConcreteType actualType' ->
                    typeCompares'' symtab (expr, actualType') expectedType'
typeCompares'' :: 
    KeliSymTab 
    -> (V.Expr', V.Type') -- actual expr
    -> V.Type' -- expected type
    -> TypeCompareResult

typeCompares'' symtab (_, (V.TypeFloat)) (V.TypeFloat) = 
    ApplicableOk symtab

typeCompares'' symtab (_, (V.TypeInt)) (V.TypeInt) = 
    ApplicableOk symtab

typeCompares'' symtab (_, (V.TypeString)) (V.TypeString) = 
    ApplicableOk symtab

typeCompares'' symtab 
    actualExpr@(_, (V.TypeCarryfulTagConstructor x _ _ _)) 
    expectedType@(V.TypeCarryfulTagConstructor y _ _ _) = 
    if x == y then ApplicableOk symtab else ApplicableFailed (KErrorTypeMismatch actualExpr expectedType)

typeCompares'' _ 
    t1@(_, V.TypeRecordConstructor kvs1) 
    t2@(V.TypeRecordConstructor kvs2) = 
    undefined

typeCompares'' _ (_, V.TypeType) (V.TypeType) = 
    undefined

-- type check generic tagged union
typeCompares'' symtab
    (actualExpr, actualType@(V.TypeTaggedUnion (V.TaggedUnion name1 _ _ (Just actualTypeParams))))
    t2@(V.TypeTaggedUnion (V.TaggedUnion name2 _ _ (Just expectedTypeParams))) = 
    if name1 == name2 then 
        -- TODO: we need to also compares 2nd and 3rd type params
        typeCompares symtab (V.Expr actualExpr (head actualTypeParams)) (head expectedTypeParams)
    else 
        ApplicableFailed (KErrorTypeMismatch (actualExpr, actualType) t2)

-- type check non-generic tagged union
typeCompares'' symtab
    t1@(_, V.TypeTaggedUnion (V.TaggedUnion name1 _ _ Nothing))    
    t2@(V.TypeTaggedUnion (V.TaggedUnion name2 _ _ Nothing)) =
        if name1 == name2 then 
            ApplicableOk symtab
        else 
            ApplicableFailed (KErrorTypeMismatch t1 t2)

-- record type is handled differently, because we want to have structural typing
-- NOTE: kts means "key-type pairs"
typeCompares'' symtab (V.Record kvs, V.TypeRecord kts1) (V.TypeRecord kts2) = 
    let actualKeys = map fst kts1 in
    let expectedKeys = map fst kts2 in
    let expectedTypes = map snd kts2 in
    let actualValues = map snd kvs in 
    -- TODO: get the set difference of expectedKeys with actualKeys
    case match actualKeys expectedKeys of
        PerfectMatch ->
            foldl'
                (\result (key, actualExpr, expectedType) ->
                    case result of 
                        ApplicableFailed err ->
                            -- TODO:(KErrorPropertyTypeMismatch key expectedType actualType)
                            ApplicableFailed err

                        ApplicableOk tempSymtab ->
                            typeCompares tempSymtab actualExpr expectedType

                        NotApplicable tempSymtab ->
                            typeCompares tempSymtab actualExpr expectedType)

                (NotApplicable symtab) -- initial value
                (zip3 expectedKeys actualValues expectedTypes)

        GotDuplicates duplicates ->
            ApplicableFailed (KErrorDuplicatedProperties duplicates)

        GotExcessive excessiveProps ->
            ApplicableFailed (KErrorExcessiveProperties excessiveProps)
        
        Missing missingProps ->
            ApplicableFailed (KErrorMissingProperties (last actualValues) missingProps)

        ZeroIntersection ->
            ApplicableFailed (KErrorMissingProperties (last actualValues) (map snd expectedKeys))
        

typeCompares'' _ actualType expectedType =  ApplicableFailed (KErrorTypeMismatch actualType expectedType)

-- derigidify will turn the isRigid property of a TypeVariable to False
derigidify :: V.Type -> V.Type
derigidify t = 
    case t of
        V.TypeVariable name constraint _ ->
            V.TypeVariable name constraint False

        _ ->
            t

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


verifyKeyValuePairs :: KeliSymTab -> [Raw.StringToken] -> [Raw.Expr] -> Either KeliError [(V.StringToken, V.Expr)]
verifyKeyValuePairs symtab keys values = 
    case findDuplicates keys of
        Just duplicates ->
            Left (KErrorDuplicatedProperties duplicates)
        Nothing -> do
            typeCheckedExprs <- typeCheckExprs symtab CanBeAnything values >>= mapM extractExpr
            Right (zip keys typeCheckedExprs)

verifyKeyTypePairs :: KeliSymTab -> [Raw.StringToken] -> [Raw.Expr] -> Either KeliError [(V.StringToken, V.Type)]
verifyKeyTypePairs symtab keys types = do
    case findDuplicates keys of
        Just duplicates ->
            Left (KErrorDuplicatedProperties duplicates)
        Nothing -> do
            verifiedTypes <- mapM (typeCheckExpr symtab StrictlyAnalyzingType) types >>=  mapM extractType 
            Right (zip keys verifiedTypes)


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