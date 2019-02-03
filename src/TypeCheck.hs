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

typeCheckExpr :: KeliSymTab -> Assumption -> Raw.Expr -> Either KeliError (OneOf3 V.Expr V.Type V.Tag)
typeCheckExpr symtab assumption e = case e of 
    Raw.IncompleteFuncCall expr positionOfTheDotOperator -> do
        typeCheckedExpr <- typeCheckExpr symtab assumption expr 
        Left (KErrorIncompleteFuncCall typeCheckedExpr positionOfTheDotOperator)

    Raw.NumberExpr(pos,n) -> 
        case n of 
            Left intValue ->
                Right (First (V.Expr (V.IntExpr (pos, intValue)) V.TypeInt))

            Right doubleValue ->
                Right (First (V.Expr (V.DoubleExpr (pos, doubleValue)) V.TypeFloat))

    Raw.StringExpr (pos, str) -> 
        Right (First (V.Expr (V.StringExpr (pos, str)) V.TypeString))

    Raw.Id token@(_,id) -> 
        case lookup id symtab of 
            Just (KeliSymConst _ expr) -> 
                Right (First (V.Expr (V.Id token) (getType expr)))
        
            Just (KeliSymTag (V.CarrylessTag tag belongingType)) -> 
                (Right (First (V.Expr (V.CarrylessTagConstructor tag) belongingType)))

            Just (KeliSymTag (V.CarryfulTag tag carryType belongingType)) ->
                -- How to check if user forgot to put .carry ?
                --  Don't need to explicitly check it, the type system will handle it
                (Right (First (V.Expr (V.CarryfulTagConstructor tag carryType) (V.TypeCarryfulTagConstructor tag carryType belongingType))))
            
            Just (KeliSymType (V.TypeAlias _ t@(V.TypeRecord propTypePairs)) _) -> 
                -- Question: How are we gonna know if user is using this as type annotation or as record constructor?
                -- Answer: Using assumptions

                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (Second t))
                    CanBeAnything ->
                        (Right (First (V.Expr (V.RecordConstructor propTypePairs) (V.TypeRecordConstructor propTypePairs))))

            Just (KeliSymType (V.TypeAlias _ t@(V.TypeTagUnion name tags)) _) ->
                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (Second t))
                    CanBeAnything ->
                        (Right (First (V.Expr (V.TagConstructorPrefix) (V.TypeTagConstructorPrefix name tags))))
            
            Just (KeliSymType (V.TypeAlias _ t) _) ->
                (Right (Second t))
            
            Just (KeliSymImplicitTypeParam (V.TypeParam id' constraint)) ->
                Right (Second (V.TypeTypeParam id' constraint))

            Just (KeliSymExplicitTypeParam (V.TypeParam id' constraint)) ->
                Right (Second (V.TypeTypeParam id' constraint))

            Just (KeliSymTypeConstructor t@(V.TypeConstructor _ _ _ type')) ->
                case assumption of
                    StrictlyAnalyzingType ->
                        Right (First (V.Expr (V.TypeConstructorPrefix) (V.TypeTypeConstructor t)))

                    CanBeAnything -> 
                        case type' of
                            V.TypeTagUnion name tags ->
                                (Right (First (V.Expr (V.TagConstructorPrefix) (V.TypeTagConstructorPrefix name tags))))
                            _ ->
                                undefined

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
                            Second (V.TypeTagUnion{})-> True; 
                            Third _ -> True; 
                            _ -> False;
                params <- mapM (typeCheckExpr symtab assumption) params';
                if isTagOrUnion (head params) then do
                    tags <- mapM extractTag params
                    Right (Second (V.TypeTagUnion [] (concat tags)))
                else  do
                    continuePreprocessFuncCall1

            _ -> do  
                case (head params') of
                    (Raw.Id firstParamToken@(_,firstParamId)) -> 
                        case firstParamId of
                        -- 1. Check if user wants to create a tag
                        "tag" ->
                            case funcIds !! 0 of 
                            tagToken@(pos,"#") ->
                                if length params' < 2 then
                                    Left (KErrorIncorrectUsageOfTag tagToken)
                                else
                                    case params' !! 1 of
                                        Raw.Id tag ->
                                            -- 1.1 Check if user wants to create carryless/carryful tag
                                            if length funcIds < 2 then -- carryless tag
                                                Right (Third (V.CarrylessTag tag V.TypeUndefined))

                                            else if snd (funcIds !! 1) == "carry" then do -- carryful tag
                                                if length params' < 3 then
                                                    Left (KErrorIncorrectUsageOfTag tagToken)
                                                else do
                                                    thirdParam <- typeCheckExpr symtab StrictlyAnalyzingType (params' !! 2)
                                                    case thirdParam of
                                                        Second carryType ->
                                                            Right (Third (V.CarryfulTag tag carryType V.TypeUndefined))
                                                        
                                                        _ ->
                                                            Left (KErrorIncorrectUsageOfTag tagToken)
                                            else
                                                Left (KErrorIncorrectUsageOfTag tagToken)
                                        
                                        _ ->
                                            Left (KErrorIncorrectUsageOfTag tagToken)
                            _ ->
                                continuePreprocessFuncCall1

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
                                                            (V.TypeRecord (zip keys (map getType typeCheckedExprs)))))
                                    
                                    -- assume user want to declare a record type
                                    Second _ -> do
                                        types <- mapM (typeCheckExpr symtab StrictlyAnalyzingType) (tail params') >>=  mapM extractType 
                                        Right (Second (V.TypeRecord (zip keys types)))

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
                                        Right (First (V.Expr (V.FFIJavascript value) V.TypeUndefined))

                                    _ -> 
                                        Left (KErrorFFIValueShouldBeString jsCode)
                            
                        _ -> 
                            continuePreprocessFuncCall1

                    _ -> 
                        continuePreprocessFuncCall1

        where 
            continuePreprocessFuncCall1 :: Either KeliError (OneOf3 V.Expr V.Type V.Tag)
            continuePreprocessFuncCall1 = do
                firstParam <- typeCheckExpr symtab assumption (head params') >>= extractExpr

                let typeOfFirstParam = getType firstParam in
                    case typeOfFirstParam of
                    -- (A) check if user is invoking record constructor
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
                                                let actualType = getType (snd actual) in
                                                case typeCompares symtab actualType expectedType of
                                                    Applicable expectedTypeEqualsActualType ->
                                                        if expectedTypeEqualsActualType then
                                                            Right updatedSymtab
                                                        else
                                                            Left (KErrorPropertyTypeMismatch (fst expected) (snd expected) (snd actual))

                                                    NotApplicable newSymtab ->
                                                        Right newSymtab

                                            Left err ->
                                                Left err) 

                                    -- initial value
                                    (Right symtab)

                                    --foldee
                                    (zip expectedPropTypePairs' actualPropValuePairs) of

                                        Right _ ->
                                            Right (First (V.Expr (V.Record actualPropValuePairs) (V.TypeRecord expectedPropTypePairs')))
                                        
                                        Left err ->
                                            Left err

                                    
                    -- (B) check if user is invoking carryful tag constructor
                    V.TypeCarryfulTagConstructor tag expectedCarryType belongingType -> 
                        if length funcIds == 1 && (snd (funcIds !! 0)) == "carry" then do
                            carryExpr  <- typeCheckExpr symtab assumption (params' !! 1) >>= extractExpr
                            let actualType = getType carryExpr 
                            case typeCompares symtab actualType (pTraceShowId expectedCarryType) of
                                Applicable actualTypeEqualsExpectedType ->
                                    if actualTypeEqualsExpectedType then
                                        Right (First (V.Expr (V.CarryfulTagExpr tag carryExpr) belongingType))
                                    else
                                        Left (KErrorIncorrectCarryType expectedCarryType carryExpr)
                                
                                NotApplicable updatedSymtab ->
                                    undefined

                        else
                            continuePreprocessFuncCall2
                        
                    
                    -- (C) check if user is calling tag matchers
                    V.TypeTagUnion name expectedTags -> do
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
                                    if any 
                                        (\branch -> 
                                            case typeCompares symtab (getType branch) expectedTypeOfEachBranches of
                                                Applicable True ->
                                                    False
                                                _ ->
                                                    True) branches then
                                        Left (KErrorNotAllBranchHaveTheSameType branches)
                                    else
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
                                if any 
                                    (\branch -> 
                                        case typeCompares symtab (getType branch) expectedTypeOfEachBranches of
                                            Applicable True ->
                                                False
                                            _ ->
                                                True) branches then
                                    Left (KErrorNotAllBranchHaveTheSameType branches)
                                else
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
                                                            symtab |> (snd id, KeliSymConst id (V.Expr (V.Id id) (V.TypeIdentifiedCarryfulBranch carryType)))

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
                                        let actualType = getType newValue
                                        case typeCompares symtab actualType expectedType of
                                            Applicable True ->
                                                Right (First (V.Expr 
                                                    (V.RecordSetter subject propertyName newValue) recordType))
                                            
                                            _ -> 
                                                Left (KErrorWrongTypeInSetter newValue expectedType)

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
                    V.TypeTagConstructorPrefix taggedUnionName tags ->
                        if length funcIds == 1 then
                            let tagname = head funcIds in
                            case find (\t -> snd (V.tagnameOf t) == snd tagname) tags of
                                Just (V.CarrylessTag tag belongingType) -> 
                                    (Right (First (V.Expr (V.CarrylessTagConstructor tag) belongingType)))

                                Just (V.CarryfulTag tag carryType belongingType) ->
                                    (Right (First (V.Expr (V.CarryfulTagConstructor tag carryType) (V.TypeCarryfulTagConstructor tag carryType belongingType))))

                                Nothing ->
                                    Left (KErrorTagNotFound [tagname] taggedUnionName tags)

                        else
                            Left (KErrorIncorrectUsageOfTagConstructorPrefix e)

                    -- (G) check if user is invoking type constructor
                    V.TypeTypeConstructor (V.TypeConstructor name ids expectedTypeParams _) ->
                        if map snd funcIds /= map snd ids then
                            Left (KErrorTypeConstructorIdsMismatch funcIds)
                        else do
                            -- TODO: check if type param conforms to type constraint
                            types <- typeCheckExprs symtab StrictlyAnalyzingType (tail params') >>= mapM extractType 
                            Right (Second (V.TypeCompound name types))

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
                                (V.Expr expr' V.TypeUndefined) ->
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
                        if length funcCallParams /= length (V.funcDeclParams f) then
                            StillNoMatchingFunc
                        else do
                            let expectedParamTypes = map (\(_,paramType) -> paramType) (V.funcDeclParams f) 
                            let actualParamTypes = map (getType) funcCallParams 
                            let fisrtParamExpectedType = head expectedParamTypes
                            let firstParamActualType = head actualParamTypes 
                            -- if only have 1 param, then just check if the first param match the expected type
                            if length funcCallParams == 1 then 
                                case typeCompares symtab firstParamActualType fisrtParamExpectedType of
                                    Applicable False ->
                                        StillNoMatchingFunc

                                    Applicable True ->
                                        PerfectlyMatchedFuncFound f symtab

                                    NotApplicable updatedSymtab ->
                                        PerfectlyMatchedFuncFound f updatedSymtab

                            -- if more than 1 param, then we need to check if every param match every expected types
                            -- why is this branching necessary? 
                            --     so that we can report a better error message
                            else 
                                case typeCompares symtab firstParamActualType fisrtParamExpectedType of
                                    -- if the first param does not match expected type
                                    Applicable False ->
                                        StillNoMatchingFunc
                                    
                                    -- if the first param match the expected type, check all subsequent params
                                    _ -> 
                                        case foldM
                                                (\tempSymtab ((expr, actualType), expectedType) -> 
                                                    case typeCompares tempSymtab actualType expectedType of
                                                        Applicable False -> 
                                                            Left (KErrorFuncCallTypeMismatch expectedType expr)
                                                        
                                                        Applicable True ->
                                                            Right tempSymtab
                                                        
                                                        NotApplicable updatedSymtab ->
                                                            Right updatedSymtab)
                                                symtab
                                                (zip (zip funcCallParams actualParamTypes) expectedParamTypes) of 
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
                -- Just matchingFunc -> do
                --     let genericParams = V.funcDeclGenericParams matchingFunc

                --     -- 1. Create empty binding table
                --     let initialBindingTable = (fromList (map (\(V.TypeParam (_,id) _) -> (id, Nothing)) genericParams)) :: GenericBindingTable

                --     -- 1.1 Populate binding table
                --     let expectedParamTypes = map snd (V.funcDeclParams matchingFunc)

                --     populatedBindingTable <- 
                --             ((foldM 
                --                 ((\bindingTable2 (expectedType, actualType) -> 
                --                     let genericParamLocations = whereAreGenericParams expectedType in
                --                     case findCorrespondingType genericParamLocations actualType of
                --                         Right (CorrespondingTypeNotFound) -> 
                --                             Right bindingTable2

                --                         Right (CorrespondingTypeFound bindings) -> 
                --                             Right $
                --                                 (foldl'
                --                                 ((\bindingTable3 ((_,id), bindingType) -> 
                --                                     case lookup id bindingTable3 of
                --                                         Just Nothing -> 
                --                                             (bindingTable3 |> (id, Just bindingType)) 

                --                                         Just (Just _) -> 
                --                                             -- if already binded, don't insert the new type binding
                --                                             bindingTable3

                --                                         Nothing -> 
                --                                             error "shouldn't be possible") :: GenericBindingTable -> (Raw.StringToken, V.Type) -> GenericBindingTable)

                --                                 (bindingTable2 :: GenericBindingTable)
                --                                 bindings)

                --                         Left err -> Left err
                --                     ) :: GenericBindingTable -> (V.Type, V.Type) -> Either KeliError GenericBindingTable)
                --                 initialBindingTable
                --                 (zip expectedParamTypes actualParamTypes)))

                    
                --     -- 2. Subsitute type param bindings
                --     let specializedFunc = substituteGeneric populatedBindingTable matchingFunc 

                --     -- 3. Check if each func call param type match with param types of specializedFunc
                --     let typeMismatchError = 
                --             find
                --             (\(expectedType, actualExpr) -> not (getType actualExpr `typeCompares` expectedType))
                --             (zip (map snd (V.funcDeclParams specializedFunc)) funcCallParams)
                    
                --     case typeMismatchError of
                --         Just (expectedType, actualExpr) -> 
                --             Left (KErrorFuncCallTypeMismatch expectedType actualExpr)

                --         Nothing ->
                --             let funcCall = V.FuncCall funcCallParams funcIds matchingFunc in
                --             Right (First (V.Expr funcCall (V.funcDeclReturnType specializedFunc)))

        
        Just other ->
            Left (KErrorNotAFunction funcIds)

        _ -> 
            Left (KErrorUsingUndefinedFunc funcIds [])

resolveType :: KeliSymTab -> V.Type -> V.Type
resolveType symtab t =
    case t of
        V.TypeTypeParam name _ ->
            case lookup (snd name) symtab of
                Just (KeliSymType (V.TypeAlias _ boundedType) []) ->
                    boundedType

                _ ->
                    error "should be impossible"
        
        other ->
            t

substituteGeneric :: GenericBindingTable -> V.Func -> V.Func
substituteGeneric bindingTable matchingFunc = 
    let expectedFuncParams = V.funcDeclParams matchingFunc in
    let substitutedFuncParams = 
            map (\(paramId, paramType) -> (paramId, substituteGeneric' bindingTable paramType)) expectedFuncParams in

    let substitutedReturnType = substituteGeneric' bindingTable (V.funcDeclReturnType matchingFunc) in

    (matchingFunc {
        V.funcDeclParams = substitutedFuncParams, 
        V.funcDeclReturnType = substitutedReturnType})
    
substituteGeneric' :: GenericBindingTable -> V.Type -> V.Type
substituteGeneric' bindingTable type' =
    case type' of
        V.TypeTypeParam (_,id) _ -> 
            case lookup id bindingTable of 
                Just (Just bindingType) -> bindingType
                _ -> error "possibly due to binding table is not populated properly"
        
        V.TypeCompound _ _ -> 
            undefined

        _ -> 
            type'


verifyType :: KeliSymTab -> Raw.Expr -> Either KeliError V.Type
verifyType symtab expr = typeCheckExpr symtab StrictlyAnalyzingType expr >>= extractType

verifyTypeParam :: KeliSymTab -> (Raw.StringToken, Raw.Expr) -> Either KeliError V.TypeParam
verifyTypeParam symtab (paramName, expr) = do
    result <- typeCheckExpr symtab StrictlyAnalyzingType expr
    case result of
        Second V.TypeType ->
            Right (V.TypeParam paramName Nothing)
        _ ->
            Left (KErrorInvalidTypeParamDecl expr)
    -- case expr of
    --     Raw.Id (_,name) ->
    --         case lookup name symtab of
    --             Just (KeliSymTypeConstraint _ constraint) -> 
    --                 Right constraint
                
    --             _ ->
    --                 Left (KErrorExprIsNotATypeConstraint expr)


    --     _ ->
    --         undefined

typeCheckExprs :: KeliSymTab -> Assumption -> [Raw.Expr] -> Either KeliError [OneOf3 V.Expr V.Type V.Tag]
typeCheckExprs symtab assumption exprs = mapM (typeCheckExpr symtab assumption) exprs


    
-- Example
--  a.list `haveShapeOf` b.list = True
--  a.list.list `haveShapeOf` b.list.list = True
--  a.list `haveShapeOf` a.tree = False
--  Int `haveShapeOf` Int = True
--  Int `haveShapeOf` a = True
-- haveShapeOf :: V.Type -> V.Type -> Bool
-- type1 `haveShapeOf` type2 = 
--     case type1 of
--         V.TypeCompound _ _ -> 
--             undefined
        
--         _ -> 
--             case type2 of
--                 V.TypeTypeParam _ _ -> 
--                     True
                
--                 _ ->
--                     type1 `typeCompares` type2

hardConformsTo :: V.Type -> (Maybe V.TypeConstraint) -> Bool
type' `hardConformsTo` Nothing = True
type' `hardConformsTo` (Just constraint) = 
    case constraint of
        V.ConstraintAny -> 
            True
        _ -> 
            undefined

data GenericParamLocation 
    = GenericParamNotFound
    | GenericParamFoundAsSimpleType
        V.StringToken-- generic param name
        (Maybe V.TypeConstraint)

    | GenericParamFoundAsCompoundType
        [(
            Int, -- generic param index,
            GenericParamLocation
        )]
    deriving (Show)

whereAreGenericParams :: V.Type -> GenericParamLocation
whereAreGenericParams t = case t of
    -- compound type 
    V.TypeCompound _ _ -> undefined

    -- simple type
    V.TypeTypeParam id constraint -> GenericParamFoundAsSimpleType id constraint

    _ -> GenericParamNotFound

data CorrespondingType
    = CorrespondingTypeNotFound
    | CorrespondingTypeFound [(Raw.StringToken, V.Type)]
    deriving(Show)

findCorrespondingType :: GenericParamLocation -> V.Type -> Either KeliError CorrespondingType
findCorrespondingType location actualType =
    case location of
        GenericParamNotFound -> 
            Right CorrespondingTypeNotFound

        GenericParamFoundAsSimpleType genericParamId constraint -> 
            if actualType `hardConformsTo` constraint then
                Right (CorrespondingTypeFound [(genericParamId, actualType)])

            else
                Left (KErrorTypeNotConformingConstraint actualType (fromJust constraint))

        GenericParamFoundAsCompoundType _ -> undefined

type GenericBindingTable = OMap String (Maybe V.Type) 

extractTag :: OneOf3 V.Expr V.Type V.Tag -> Either KeliError [V.Tag]
extractTag x =
    case x of
        First expr -> Left (KErrorExpectedTagButGotExpr expr)
        Second (V.TypeTagUnion _ tags) -> Right tags
        Second type' -> Left (KErrorExpectedTagButGotType type')
        Third tag -> Right [tag]

extractType :: OneOf3 V.Expr V.Type V.Tag -> Either KeliError V.Type
extractType x = 
    case x of
        First expr   -> Left (KErrorExpectedTypeButGotExpr expr)
        Second type' -> Right type' 
        Third tag    -> Left (KErrorExpectedTypeButGotTag tag)



extractExpr :: OneOf3 V.Expr V.Type V.Tag -> Either KeliError V.Expr
extractExpr x = 
    case x of 
        First expr -> Right expr
        Second type' -> Left (KErrorExpectedExprButGotType type')
        Third tag -> Left (KErrorExpectedExprButGotTag tag)

data TypeCompareResult
    = Applicable -- if both operands are not type parameters OR both operands are type parameters (with the same name)
        Bool

    | NotApplicable -- if one of the operand is a type parameter
        KeliSymTab  --  an updated environment, which binds the type parameter to the comparer type

typeCompares :: 
    KeliSymTab 
    -> V.Type -- actual type
    -> V.Type -- expected type
    -> TypeCompareResult

typeCompares symtab actualType expectedType =
    case expectedType of 
        V.TypeTypeParam name1 constraint1 ->
            case actualType of
                -- if both actualType and expectedType are type params, just compare their name
                V.TypeTypeParam name2 constraint2 ->
                    Applicable (snd name1 == snd name2)
                    -- TODO: should I check if actualType is also bounded?

                -- if expectedType is a bounded type param AND actualType is a concrete type
                _ ->
                    -- lookup symtab to check if this type parameter is already bounded to a type 
                    case lookup (snd name1) symtab of
                        -- if bounded, compare actualType to boundedType
                        Just (KeliSymType (V.TypeAlias _ boundedType) []) ->
                            Applicable (actualType == boundedType)

                        -- if not bounded, update the environment (bound actualType to the type param)
                        Nothing ->
                            NotApplicable (symtab |> (snd name1, KeliSymType (V.TypeAlias [name1] actualType) []))

        -- if both are concrete types, just do direct comparison
        _ ->
            Applicable (actualType == expectedType)