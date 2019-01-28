{-# LANGUAGE BangPatterns #-}
module TypeCheck where


import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered (OMap, (|>), lookup, fromList) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)
import Data.Maybe (catMaybes, fromJust)

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError
import Symbol
import Util

data OneOf3 a b c = First a | Second b | Third c deriving (Show)

getType :: V.Expr -> V.Type
getType (V.Expr _ type') = type'

data Assumption 
    = StrictlyAnalyzingType
    | CanBeAnything 

typeCheckExpr :: KeliSymTab -> Assumption -> Raw.Expr -> Either KeliError (OneOf3 V.Expr V.Type V.Tag)
typeCheckExpr symtab assumption e = case e of 
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
            
            Just (KeliSymType (V.TypeAlias _ t@(V.TypeRecord propTypePairs))) -> 
                -- Question: How are we gonna know if user is using this as type annotation or as record constructor?
                -- Answer: Using assumptions

                case assumption of
                    StrictlyAnalyzingType -> 
                        (Right (Second t))
                    CanBeAnything ->
                        (Right (First (V.Expr (V.RecordConstructor propTypePairs) (V.TypeRecordConstructor propTypePairs))))
            
            Just (KeliSymType (V.TypeAlias _ t)) ->
                (Right (Second t))
            
            Just (KeliSymTypeParam id constraint) ->
                Right (Second (V.TypeParam id constraint))

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
                    Right (Second (V.TypeTagUnion V.nullStringToken (concat tags)))
                else  do
                    continuePreprocessFuncCall

            _ -> do  
                case (head params') of
                    (Raw.Id firstParamToken@(_,firstParamId)) -> 
                        case firstParamId of
                        -- 1. Check if user wants to create a tag
                        "_" ->
                            case funcIds !! 0 of 
                            (pos,"tag") ->
                                if length params' < 2 then
                                    Left (KErrorIncorrectUsageOfTag pos)
                                else
                                    case params' !! 1 of
                                        Raw.Id tag ->
                                            -- 1.1 Check if user wants to create carryless/carryful tag
                                            if length funcIds < 2 then -- carryless tag
                                                Right (Third (V.CarrylessTag tag V.TypeUndefined))

                                            else if snd (funcIds !! 1) == "carry" then do -- carryful tag
                                                if length params' < 3 then
                                                    Left (KErrorIncorrectUsageOfTag pos)
                                                else do
                                                    thirdParam <- typeCheckExpr symtab StrictlyAnalyzingType (params' !! 2)
                                                    case thirdParam of
                                                        Second carryType ->
                                                            Right (Third (V.CarryfulTag tag carryType V.TypeUndefined))
                                                        
                                                        _ ->
                                                            Left (KErrorIncorrectUsageOfTag pos)
                                            else
                                                Left (KErrorIncorrectUsageOfTag pos)
                                        
                                        _ ->
                                            Left (KErrorIncorrectUsageOfTag pos)
                            _ ->
                                continuePreprocessFuncCall

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
                                    First _ -> do
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
                                Left (KErrorIncorrectUsageOfFFI (fst firstParamToken))
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
                            continuePreprocessFuncCall

                    _ -> 
                        continuePreprocessFuncCall

        where 
            continuePreprocessFuncCall :: Either KeliError (OneOf3 V.Expr V.Type V.Tag)
            continuePreprocessFuncCall = do
                firstParam <- typeCheckExpr symtab assumption (head params') >>= extractExpr

                let typeOfFirstParam = getType firstParam in
                    case typeOfFirstParam of
                    -- (A) check if user is invoking record constructor
                    V.TypeRecordConstructor expectedPropTypePairs -> do
                        let expectedProps = map fst expectedPropTypePairs
                        let actualProps = funcIds 
                        case match actualProps expectedProps of
                            GotDuplicates ->
                                Left KErrorDuplicatedProperties

                            ZeroIntersection ->
                                treatAsNormalFuncCall
                            
                            GotExcessive excessiveProps ->
                                Left (KErrorExcessiveProperties excessiveProps)
                            
                            Missing missingProps ->
                                Left (KErrorMissingProperties missingProps)
                            
                            PerfectMatch ->  do
                                values  <- typeCheckExprs symtab assumption (tail params') >>= mapM extractExpr 

                                let verifiedPropValuePairs = zip funcIds values

                                let expectedPropTypePairs' = sortBy (\((_,a),_) ((_,b),_) -> compare a b) expectedPropTypePairs 
                                let actualPropValuePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) verifiedPropValuePairs 
                                case find (\(expected, actual) -> 
                                        let expectedType = snd expected in
                                        let actualType = getType (snd actual) in
                                        not (expectedType `V.typeEquals` actualType)
                                    ) (zip expectedPropTypePairs' actualPropValuePairs) of

                                    Just (expected, actual) -> 
                                        Left (KErrorPropretyTypeMismatch (fst expected) (snd expected) (snd actual))
                                    Nothing -> 
                                        Right (First (V.Expr (V.Record actualPropValuePairs) (V.TypeRecord expectedPropTypePairs')))

                                    
                    -- (B) check if user is invoking carryful tag constructor
                    V.TypeCarryfulTagConstructor tag carryType belongingType -> 
                        if length funcIds == 1 && (snd (funcIds !! 0)) == "carry" then do
                            carryExpr  <- typeCheckExpr symtab assumption (params' !! 1) >>= extractExpr
                            let carryExprType = getType carryExpr 
                            let carryType'    = carryType 
                            if carryExprType `V.typeEquals` carryType' then
                                Right (First (V.Expr (V.CarryfulTagExpr tag carryExpr) belongingType))
                            else
                                Left (KErrorIncorrectCarryType carryType' carryExpr)
                        else
                            treatAsNormalFuncCall
                        
                    
                    -- (C) check if user is calling tag matchers
                    V.TypeTagUnion _ expectedTags -> do
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
                            GotDuplicates -> 
                                Left (KErrorDuplicatedTags funcIds)

                            ZeroIntersection -> 
                                treatAsNormalFuncCall
                            
                            GotExcessive excessiveCases ->
                                Left (KErrorExcessiveTags excessiveCases)
                            
                            Missing cases -> do
                                tagBranches <- getTagBranches subject
                                let branches = map snd tagBranches
                                let firstBranch = head branches 
                                if "else?" `elem` (map snd funcIds) then
                                    let elseBranch = fromJust (find (\((_,id),_) -> id == "else?") tagBranches) in
                                    let otherBranches = filter (\((_,id),_) -> id /= "else?") tagBranches in
                                    if any (\x -> not (getType x `V.typeEquals` getType firstBranch)) branches then
                                        Left (KErrorNotAllBranchHaveTheSameType branches)
                                    else
                                        Right (First (V.Expr 
                                            (V.TagMatcher 
                                                subject 
                                                otherBranches 
                                                (Just (snd elseBranch)))
                                            (getType (head branches))))


                                else -- missing tags
                                    Left (KErrorMissingTags cases)

                            PerfectMatch -> do
                                tagBranches <- getTagBranches subject
                                let branches = map snd tagBranches
                                let firstBranch = head branches 

                                -- check if all branch have the same type
                                if any (\x -> not (getType x `V.typeEquals` getType firstBranch)) branches then
                                    Left (KErrorNotAllBranchHaveTheSameType branches)
                                else
                                    Right (First (V.Expr 
                                        (V.TagMatcher 
                                            subject 
                                            tagBranches
                                            Nothing) 
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
                            treatAsNormalFuncCall
                        else
                            let subject = firstParam in
                            case find (\((_,key),_) -> key == snd (funcIds !! 0)) kvs of
                                Just (propertyName, expectedType) -> 
                                    -- Check if is getter or setter
                                    if length (tail params') == 0
                                    then -- is getter
                                        Right (First (V.Expr (V.RecordGetter subject propertyName) expectedType))
                                    else do -- is setterhi
                                        newValue <- typeCheckExpr symtab assumption ((tail params') !! 0) >>= extractExpr
                                        if getType newValue `V.typeEquals` expectedType then
                                            Right 
                                                (First 
                                                    (V.Expr 
                                                        (V.RecordSetter subject propertyName newValue) recordType))
                                        else
                                            Left (KErrorWrongTypeInSetter)

                                Nothing -> 
                                    treatAsNormalFuncCall


                    -- (E) check if user is retrieving the carry of an identified tag branch
                    V.TypeIdentifiedCarryfulBranch carryType ->
                        if length funcIds == 1 && snd (funcIds !! 0) == "carry" then
                            let subject = firstParam in
                            (Right (First (V.Expr (V.RetrieveCarryExpr subject) carryType)))
                        else
                            Left (KErrorIncorrectMethodToRetrieveCarry (funcIds !! 0))

                    _ ->
                        treatAsNormalFuncCall
            
            treatAsNormalFuncCall = do
                params <- typeCheckExprs symtab assumption params' >>= mapM extractExpr
                typeCheckFuncCall symtab params funcIds

    Raw.AnnotatedExpr expr annotatedType -> do
        verifiedExpr <- typeCheckExpr symtab assumption expr >>= extractExpr
        verifiedType <- typeCheckExpr symtab StrictlyAnalyzingType annotatedType >>= extractType
        case verifiedExpr of
            (V.Expr expr' V.TypeUndefined) ->
                Right (First (V.Expr expr' verifiedType))

            _ ->
                undefined

    other -> 
        undefined

    where 

-- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
typeCheckFuncCall :: KeliSymTab -> [V.Expr] -> [Raw.StringToken] -> Either KeliError (OneOf3 V.Expr V.Type V.Tag)
typeCheckFuncCall symtab funcCallParams funcIds = 
    let funcId = intercalate "$" (map snd funcIds) in
    let actualParamTypes = map (getType) funcCallParams in
    case lookup funcId symtab of
        Just (KeliSymFunc candidateFuncs) -> do
            case (find 
                    (\f ->
                        if length funcCallParams /= length (V.funcDeclParams f) then
                            False
                        else
                            let expectedParamTypes = map (\(_,paramType) -> paramType) (V.funcDeclParams f) in
                            all 
                                (\(actualType, expectedType) -> actualType `haveShapeOf` expectedType) 
                                (zip actualParamTypes expectedParamTypes))

                    -- This sorting is necessary so that the compiler will look for more specialized (a.k.a less generic) function first
                    (sortOn (\f -> length (V.funcDeclGenericParams f)) candidateFuncs)) of

                Just matchingFunc -> do
                    let genericParams = V.funcDeclGenericParams matchingFunc

                    -- 1. Create empty binding table
                    let initialBindingTable = (fromList (map (\((_,id),_) -> (id, Nothing)) genericParams)) :: GenericBindingTable

                    -- 1.1 Populate binding table
                    let expectedParamTypes = map snd (V.funcDeclParams matchingFunc)

                    populatedBindingTable <- 
                            ((foldM 
                                ((\bindingTable2 (expectedType, actualType) -> 
                                    let genericParamLocations = whereAreGenericParams expectedType in
                                    case findCorrespondingType genericParamLocations actualType of
                                        Right (CorrespondingTypeNotFound) -> 
                                            Right bindingTable2

                                        Right (CorrespondingTypeFound bindings) -> 
                                            Right $
                                                (foldl'
                                                ((\bindingTable3 ((_,id), bindingType) -> 
                                                    case lookup id bindingTable3 of
                                                        Just Nothing -> 
                                                            (bindingTable3 |> (id, Just bindingType)) 

                                                        Just (Just _) -> 
                                                            -- if already binded, don't insert the new type binding
                                                            bindingTable3

                                                        Nothing -> 
                                                            error "shouldn't be possible") :: GenericBindingTable -> (Raw.StringToken, V.Type) -> GenericBindingTable)

                                                (bindingTable2 :: GenericBindingTable)
                                                bindings)

                                        Left err -> Left err
                                    ) :: GenericBindingTable -> (V.Type, V.Type) -> Either KeliError GenericBindingTable)
                                initialBindingTable
                                (zip expectedParamTypes actualParamTypes)))

                    
                    -- 2. Subsitute type param bindings
                    let specializedFunc = substituteGeneric populatedBindingTable matchingFunc 

                    -- 3. Check if each func call param type match with param types of specializedFunc
                    let typeMismatchError = 
                            find
                            (\(expectedType, actualExpr) -> not (getType actualExpr `V.typeEquals` expectedType))
                            (zip (map snd (V.funcDeclParams specializedFunc)) funcCallParams)
                    
                    case typeMismatchError of
                        Just (expectedType, actualExpr) -> 
                            Left (KErrorFuncCallTypeMismatch expectedType actualExpr)

                        Nothing ->
                            let funcCall = V.FuncCall funcCallParams funcIds matchingFunc in
                            Right (First (V.Expr funcCall (V.funcDeclReturnType specializedFunc)))

                Nothing ->
                    Left (KErrorUsingUndefinedFunc funcIds candidateFuncs)
        
        Just other ->
            Left (KErrorNotAFunction other)

        _ -> 
            Left (KErrorUsingUndefinedFunc funcIds [])

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
        V.TypeParam (_,id) _ -> 
            case lookup id bindingTable of 
                Just (Just bindingType) -> bindingType
                _ -> error "possibly due to binding table is not populated properly"
        
        V.TypeCompound _ _ -> 
            undefined

        _ -> 
            type'


verifyType :: KeliSymTab -> Raw.Expr -> Either KeliError V.Type
verifyType symtab expr = typeCheckExpr symtab StrictlyAnalyzingType expr >>= extractType

verifyTypeConstraint :: KeliSymTab -> Raw.Expr -> Either KeliError V.TypeConstraint
verifyTypeConstraint symtab expr = 
    case expr of
        Raw.Id (_,name) ->
            case lookup name symtab of
                Just (KeliSymTypeConstraint _ constraint) -> 
                    Right constraint
                
                _ ->
                    Left (KErrorExprIsNotATypeConstraint expr)

        _ ->
            undefined

typeCheckExprs :: KeliSymTab -> Assumption -> [Raw.Expr] -> Either KeliError [OneOf3 V.Expr V.Type V.Tag]
typeCheckExprs symtab assumption exprs = mapM (typeCheckExpr symtab assumption) exprs


    
-- Example
--  a.list `haveShapeOf` b.list = True
--  a.list.list `haveShapeOf` b.list.list = True
--  a.list `haveShapeOf` a.tree = False
--  int `haveShapeOf` int = True
--  int `haveShapeOf` a = True
haveShapeOf :: V.Type -> V.Type -> Bool
type1 `haveShapeOf` type2 = 
    case type1 of
        V.TypeCompound _ _ -> 
            undefined
        
        _ -> 
            case type2 of
                V.TypeParam _ _ -> 
                    True
                
                _ ->
                    type1 `V.typeEquals` type2

hardConformsTo :: V.Type -> V.TypeConstraint -> Bool
type' `hardConformsTo` constraint =
    case constraint of
        V.ConstraintAny -> 
            True
        _ -> 
            undefined

data GenericParamLocation 
    = GenericParamNotFound
    | GenericParamFoundAsSimpleType
        V.StringToken-- generic param name
        V.TypeConstraint

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
    V.TypeParam id constraint -> GenericParamFoundAsSimpleType id constraint

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
                Left (KErrorTypeNotConformingConstraint actualType constraint)

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