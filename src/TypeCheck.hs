{-# LANGUAGE BangPatterns #-}
module TypeCheck where


import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered (OMap, (|>), lookup, fromList) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)
import Data.Maybe (fromJust)

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
typeCheckExpr symtab assumption e = case e of 
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
        
            Just (KeliSymTag (V.CarrylessTag tag (V.TaggedUnion name ids tags typeParams))) -> 
                let belongingUnion = 
                        V.TaggedUnion 
                        name 
                        ids 
                        tags 
                        (case typeParams of Just typeParams' -> Just (map derigidify typeParams'); _ -> typeParams) in
                (Right (First (V.Expr 
                    (V.CarrylessTagConstructor tag token) 
                    (V.ConcreteType (V.TypeTaggedUnion belongingUnion)))))

            Just (KeliSymTag (V.CarryfulTag tag carryType belongingUnion)) ->
                -- How to check if user forgot to put .carry ?
                --  Don't need to explicitly check it, the type system will handle it

                (Right (First (V.Expr (V.CarryfulTagConstructor tag carryType) (V.ConcreteType (V.TypeCarryfulTagConstructor tag carryType belongingUnion Nothing)))))
            
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
                        -- 1. Check if user wants to create a tag
                        "tag" ->
                                let tag = funcIds !! 0 in
                                -- 1.1 Check if user wants to create carryless tag
                                (if length params' == 1 then
                                    Right (Third [V.UnlinkedCarrylessTag tag])

                                
                                -- 1.2 Check if user wants to create carryful tag
                                else if length params' == 2 then do
                                    carryType <- typeCheckExpr symtab StrictlyAnalyzingType (params' !! 1) >>= extractType
                                    Right (Third [V.UnlinkedCarryfulTag tag carryType])
                                        
                                else 
                                    Left (KErrorIncorrectUsageOfTag firstParamToken))

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
                                -- PROPOSED SOLUTION:
                                --  Force user to annotate the type of type as such:
                                --
                                --      fruit:type = record.next fruit;
                                firstValue <- typeCheckExpr symtab CanBeAnything (tail params' !! 0)  -- <-- this line

                                let keys = funcIds
                                case firstValue of
                                    -- assume user want to create a record value
                                    First _ -> 
                                        case findDuplicates keys of
                                            Just duplicates ->
                                                Left (KErrorDuplicatedProperties duplicates)
                                            Nothing -> do
                                                typeCheckedExprs <- typeCheckExprs symtab CanBeAnything (tail params') >>= mapM extractExpr
                                                Right 
                                                    (First
                                                        (V.Expr
                                                            (V.Record (zip keys typeCheckedExprs)) 
                                                            (V.ConcreteType (V.TypeRecord (zip keys (map getType typeCheckedExprs))))))
                                    
                                    -- assume user want to declare a record type
                                    Second _ -> do
                                        types <- mapM (typeCheckExpr symtab StrictlyAnalyzingType) (tail params') >>=  mapM extractType 
                                        Right (Second (V.ConcreteType (V.TypeRecord (zip keys types))))

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
                                                            ApplicableOk  ->
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

                                            
                            -- (B) check if user is invoking carryful tag constructor
                            V.TypeCarryfulTagConstructor tag expectedCarryType belongingUnion@(V.TaggedUnion name ids tags _) typeParams -> 
                                if length funcIds == 1 && (snd (funcIds !! 0)) == "carry" then do
                                    carryExpr  <- typeCheckExpr symtab assumption (params' !! 1) >>= extractExpr
                                    case typeCompares symtab carryExpr expectedCarryType of
                                        ApplicableOk ->
                                            Right (First (V.Expr (V.CarryfulTagExpr tag carryExpr) (V.ConcreteType (V.TypeTaggedUnion belongingUnion))))
                                        
                                        ApplicableFailed err ->
                                            Left err
                                            -- Left (KErrorIncorrectCarryType expectedCarryType carryExpr)
                                        
                                        NotApplicable updatedSymtab ->
                                            case typeParams of
                                                Just typeParams' ->
                                                    let resolvedTypeParams = map (resolveType updatedSymtab) typeParams' in
                                                    Right (First (V.Expr 
                                                        (V.CarryfulTagExpr tag carryExpr) (V.ConcreteType 
                                                        (V.TypeTaggedUnion (V.TaggedUnion name ids tags (Just resolvedTypeParams))))))
                                                _ ->
                                                    Right (First (V.Expr 
                                                        (V.CarryfulTagExpr tag carryExpr) (V.ConcreteType 
                                                        (V.TypeTaggedUnion (V.TaggedUnion name ids tags typeParams)))))

                                else
                                    continuePreprocessFuncCall2
                                
                            
                            -- (C) check if user is calling tag matchers
                            V.TypeTaggedUnion (V.TaggedUnion name _ expectedTags _) -> do
                                let subject = firstParam 
                                let tagsWithQuestionMark = map 
                                        (\(pos,id) -> (pos,id ++ "?")) 
                                        (map 
                                            (\tag -> case tag of
                                                V.CarryfulTag id _ _ -> id
                                                V.CarrylessTag id _  -> id) 
                                            expectedTags)

                                -- check if there are errors in the tags
                                case match funcIds tagsWithQuestionMark of
                                    GotDuplicates duplicates -> 
                                        Left (KErrorDuplicatedTags duplicates)

                                    ZeroIntersection -> 
                                        continuePreprocessFuncCall2
                                    
                                    GotExcessive excessiveCases ->
                                        Left (KErrorExcessiveTags excessiveCases name)
                                    
                                    Missing cases -> do
                                        tagBranches <- getTagBranches subject
                                        let branches = map snd tagBranches
                                        let firstBranch = head branches 
                                        if "else?" `elem` (map snd funcIds) then
                                            let elseBranch = fromJust (find (\((_,id),_) -> id == "else?") tagBranches) in
                                            let otherBranches = filter (\((_,id),_) -> id /= "else?") tagBranches in
                                            let expectedTypeOfEachBranches = getType firstBranch in
                                            case (foldM 
                                                (\() branch -> 
                                                    case typeCompares symtab branch expectedTypeOfEachBranches of
                                                        ApplicableFailed err ->
                                                            Left err

                                                        NotApplicable _ ->
                                                            Left (KErrorCannotMatchConcreteTypeWithRigidTypeVariable branch expectedTypeOfEachBranches)
                                                        
                                                        ApplicableOk ->
                                                            Right ()) () branches) :: Either KeliError ()  of

                                                Left err ->
                                                    Left err
                                                -- Left (KErrorNotAllBranchHaveTheSameType branches)
                                                Right () ->
                                                    Right (First (V.Expr 
                                                        (V.TagMatcher subject otherBranches (Just (snd elseBranch)))
                                                        (getType (head branches))))


                                        else -- missing tags
                                            Left (KErrorMissingTags subject cases)

                                    PerfectMatch -> do
                                        tagBranches <- getTagBranches subject
                                        let branches = map snd tagBranches
                                        let firstBranch = head branches 

                                        -- check if all branch have the same type
                                        let expectedTypeOfEachBranches = getType firstBranch 
                                        case (foldM 
                                            (\() branch -> 
                                                case typeCompares symtab branch expectedTypeOfEachBranches of
                                                    ApplicableFailed err ->
                                                        Left err

                                                    NotApplicable _ ->
                                                        Left (KErrorCannotMatchConcreteTypeWithRigidTypeVariable branch expectedTypeOfEachBranches)
                                                    
                                                    ApplicableOk ->
                                                        Right ()) () branches) :: Either KeliError ()  of
                                            
                                            Left err ->
                                                Left err
                                                -- Left (KErrorNotAllBranchHaveTheSameType branches)

                                            Right () ->
                                                Right (First (V.Expr 
                                                    (V.TagMatcher subject tagBranches Nothing) 
                                                    (getType (head branches))))
                                where
                                    -- update the symtab for each branch
                                    -- because each branch will have a different carry type
                                    getTagBranches subject =
                                        mapM 
                                            (\(tag, branch) ->  
                                                let updatedSymtab = 
                                                        case subject of 
                                                        -- we only update the symtab if the subject is an identifier
                                                        V.Expr (V.Id id) _ -> 
                                                            case find 
                                                                (\t -> case t of 
                                                                    V.CarryfulTag tagname _ _ -> snd tag == (snd tagname ++ "?")
                                                                    V.CarrylessTag {} -> False) 
                                                                expectedTags of

                                                                -- if the current branch indiciates a carryful tag
                                                                -- update the symtab 
                                                                Just (V.CarryfulTag _ carryType _) -> 
                                                                    symtab |> (snd id, KeliSymConst id (V.Expr (V.Id id) (V.ConcreteType (V.TypeIdentifiedCarryfulBranch carryType))))

                                                                -- else just return back the same symtab
                                                                _ ->
                                                                    symtab

                                                        -- else just return back the same symtab
                                                        _ ->
                                                            symtab 
                                                in do
                                                    typeCheckedBranch <- typeCheckExpr updatedSymtab assumption branch >>= extractExpr
                                                    return (tag, typeCheckedBranch)) 
                                            (zip funcIds (tail params')) 



                            -- (D) check if user is calling record getter/setter
                            recordType@(V.TypeRecord kvs) ->  
                                if length funcIds > 1 then
                                    continuePreprocessFuncCall2
                                else
                                    let subject = firstParam in
                                    case find (\((_,key),_) -> key == snd (funcIds !! 0)) kvs of
                                        Just (propertyName, expectedType) -> 
                                            -- Check if is getter or setter
                                            if length (tail params') == 0
                                            then -- is getter
                                                Right (First (V.Expr (V.RecordGetter subject propertyName) expectedType))
                                            else do -- is setter
                                                newValue <- typeCheckExpr symtab assumption ((tail params') !! 0) >>= extractExpr
                                                case typeCompares symtab newValue expectedType of
                                                    ApplicableFailed err ->
                                                        Left err

                                                    _ ->
                                                        Right (First (V.Expr 
                                                            (V.RecordSetter subject propertyName newValue) (V.ConcreteType recordType)))

                                        Nothing -> 
                                            continuePreprocessFuncCall2


                            -- (E) check if user is retrieving the carry of an identified tag branch
                            V.TypeIdentifiedCarryfulBranch carryType ->
                                if length funcIds == 1 && snd (funcIds !! 0) == "carry" then
                                    let subject = firstParam in
                                    (Right (First (V.Expr (V.RetrieveCarryExpr subject) carryType)))
                                else
                                    Left (KErrorIncorrectMethodToRetrieveCarry (funcIds !! 0))


                            -- (F) check if user is invoking tag constructor prefix
                            V.TypeTagConstructorPrefix taggedUnionName tags typeParams ->
                                if length funcIds == 1 then
                                    let tagname = head funcIds in
                                    case find (\t -> snd (V.tagnameOf t) == snd tagname) tags of
                                        Just (V.CarrylessTag tag belongingUnion) -> 
                                            case typeParams of
                                                Just typeParams' ->
                                                    (Right (First (V.Expr 
                                                        (V.CarrylessTagConstructor tag tagname) 
                                                        (V.ConcreteType (V.TypeTaggedUnion belongingUnion)))))

                                                Nothing ->
                                                    (Right (First (V.Expr 
                                                        (V.CarrylessTagConstructor tag tagname) 
                                                        (V.ConcreteType (V.TypeTaggedUnion belongingUnion)))))

                                        Just (V.CarryfulTag tag carryType belongingUnion) ->
                                            (Right (First (V.Expr 
                                                (V.CarryfulTagConstructor tag carryType) 
                                                (V.ConcreteType (V.TypeCarryfulTagConstructor tag carryType belongingUnion typeParams)))))

                                        Nothing ->
                                            Left (KErrorTagNotFound tagname taggedUnionName tags)

                                else
                                    Left (KErrorIncorrectUsageOfTagConstructorPrefix e)

                            -- (G) check if user is invoking type constructor
                            V.TypeTypeConstructor t@(V.TaggedUnion name ids tags expectedTypeParams) ->
                                if map snd funcIds /= map snd ids then
                                    Left (KErrorTypeConstructorIdsMismatch funcIds)
                                else do
                                    -- TODO: check if type param conforms to type constraint
                                    types <- typeCheckExprs symtab StrictlyAnalyzingType (tail params') >>= mapM extractType 
                                    Right (Second (V.ConcreteType (V.TypeTaggedUnion t)))

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

                                        ApplicableOk ->
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
                                                            
                                                            ApplicableOk ->
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

                other ->
                    error (show other)
                    -- error "should be impossible"

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
        First expr   -> Right expr
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
    
    | ApplicableFailed -- failed means actual type does not equals expected type
        KeliError

    | NotApplicable
        KeliSymTab  --  an updated environment, which binds the type parameter to the comparer type

instance Show TypeCompareResult where
    show ApplicableOk = "ApplicableOk"
    show ApplicableFailed{} = "ApplicableFailed"
    show NotApplicable{} = "NotApplicable"

typeCompares :: 
    KeliSymTab 
    -> V.Expr -- actual expr
    -> V.Type -- expected type
    -> TypeCompareResult

typeCompares symtab (V.Expr expr actualType) expectedType =
    case expectedType of 
        V.TypeVariable name1 constraint1 isRigid1 ->
            case actualType of
                -- if both actualType and expectedType are type params, just compare their name
                typeVar2@(V.TypeVariable name2 constraint2 isRigid2) ->
                    if snd name1 == snd name2 then
                        ApplicableOk
                    else
                        undefined
                    -- TODO: should I check if actualType is also bounded?

                -- if expectedType is a bounded type param AND actualType is a concrete type
                V.ConcreteType actualType' ->
                    -- lookup symtab to check if this type parameter is already bounded to a type 
                    case lookup (snd name1) symtab of
                        -- if bounded, compare actualType to boundedType
                        Just (KeliSymType (V.TypeAlias _ (V.ConcreteType boundedType))) ->
                            typeCompares' symtab (expr, actualType') boundedType

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
                    error (show expr ++ show expectedType')
                    -- if isRigid then
                    --     undefined
                    -- else
                    --     undefined

                -- if both are concrete types, just do direct comparison
                V.ConcreteType actualType' ->
                    typeCompares' symtab (expr, actualType') expectedType'
typeCompares' :: 
    KeliSymTab 
    -> (V.Expr', V.Type') -- actual expr
    -> V.Type' -- expected type
    -> TypeCompareResult

typeCompares' _ (_, (V.TypeFloat)) (V.TypeFloat) = 
    ApplicableOk

typeCompares' _ (_, (V.TypeInt)) (V.TypeInt) = 
    ApplicableOk

typeCompares' _ (_, (V.TypeString)) (V.TypeString) = 
    ApplicableOk

typeCompares' _ 
    actualExpr@(_, (V.TypeCarryfulTagConstructor x _ _ _)) 
    expectedType@(V.TypeCarryfulTagConstructor y _ _ _) = 
    if x == y then ApplicableOk else ApplicableFailed (KErrorTypeMismatch actualExpr expectedType)

typeCompares' _ 
    t1@(_, V.TypeRecordConstructor kvs1) 
    t2@(V.TypeRecordConstructor kvs2) = 
    undefined

typeCompares' _ (_, V.TypeType) (V.TypeType) = 
    undefined

-- type check generic tagged union
typeCompares' symtab
    (actualExpr, actualType@(V.TypeTaggedUnion (V.TaggedUnion name1 _ _ (Just actualTypeParams))))
    t2@(V.TypeTaggedUnion (V.TaggedUnion name2 _ _ (Just expectedTypeParams))) = 
    if name1 == name2 then 
        -- TODO: we need to also compares 2nd and 3rd type params
        typeCompares symtab (V.Expr actualExpr (head actualTypeParams)) (head expectedTypeParams)
    else 
        ApplicableFailed (KErrorTypeMismatch (actualExpr, actualType) t2)

-- type check non-generic tagged union
typeCompares' symtab
    t1@(_, V.TypeTaggedUnion (V.TaggedUnion name1 _ _ Nothing))    
    t2@(V.TypeTaggedUnion (V.TaggedUnion name2 _ _ Nothing)) =
        if name1 == name2 then 
            ApplicableOk
        else 
            ApplicableFailed (KErrorTypeMismatch t1 t2)

-- record type is handled differently, because we want to have structural typing
-- NOTE: kts means "key-type pairs"
typeCompares' symtab (V.Record kvs, V.TypeRecord kts1) (V.TypeRecord kts2) = 
    let actualKeys = map fst kts1 in
    let expectedKeys = map fst kts2 in
    let expectedTypes = map snd kts2 in
    let actualValues = map snd kvs in 
    let actualTypes = map snd kts1 in
    case match actualKeys expectedKeys of
        PerfectMatch ->
            case foldM
                (\tempSymtab (key, actualExpr, expectedType) ->
                    case typeCompares tempSymtab actualExpr expectedType of
                        ApplicableFailed err ->
                            -- TODO:(KErrorPropertyTypeMismatch key expectedType actualType)
                            Left err

                        ApplicableOk ->
                            Right tempSymtab
                        
                        NotApplicable updatedSymtab ->
                            Right updatedSymtab)
                symtab 
                (zip3 expectedKeys actualValues expectedTypes) of

            Left err ->
                ApplicableFailed err

            Right updatedSymtab -> 
                -- TODO: refactor is needed
                --  should recude 3 cases to 2 cases 
                NotApplicable updatedSymtab
            
        
        GotDuplicates duplicates ->
            undefined

        ZeroIntersection ->
            undefined
            
        GotExcessive excessiveCases ->
            undefined

        Missing property ->
            undefined

typeCompares' _ actualType expectedType =  ApplicableFailed (KErrorTypeMismatch actualType expectedType)

-- derigidify will turn the isRigid property of a TypeVariable to False
derigidify :: V.Type -> V.Type
derigidify t = 
    case t of
        V.TypeVariable name constraint _ ->
            V.TypeVariable name constraint False

        _ ->
            t