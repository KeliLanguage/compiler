module TypeCheck where

import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered (OMap, (|>), assocs, member, lookup, empty, fromList) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)
import Data.Maybe (catMaybes, fromJust)

import Ast
import StaticError
import Symbol
import Util

typeCheckExpr :: KeliSymTab -> KeliExpr -> Either KeliError KeliExpr
typeCheckExpr symtab expr = 
    case typeCheckExpr' symtab expr of 
        Right x@KeliTypeCheckedExpr{} -> 
            Right x

        Right _ -> 
            error ("return type of typeCheckExpr' should be KeliTypeCheckedExpr but received " ++ show expr)

        Left err -> 
            Left err


typeCheckExpr' :: KeliSymTab -> KeliExpr -> Either KeliError KeliExpr
typeCheckExpr' symtab e = case e of 
    (expr@(KeliNumber(_,n))) -> 
        (Right(KeliTypeCheckedExpr expr (case n of Left _ -> KeliTypeInt; Right _ -> KeliTypeFloat)))

    (expr@(KeliString _)) -> 
        (Right(KeliTypeCheckedExpr expr KeliTypeString))

    (expr@(KeliTypeCheckedExpr _ _)) -> 
        (Right expr)

    (expr@(KeliId token@(_,id))) -> 
        case lookup id symtab of 
            Just (KeliSymConst (KeliConst _ (KeliTypeCheckedExpr _ exprType) _)) -> 
                (Right (KeliTypeCheckedExpr expr exprType))
        
            Just (KeliSymSingleton token') -> 
                (Right (KeliTypeCheckedExpr expr (KeliTypeSingleton token')))

            Just (KeliSymTag (KeliTagCarryless tag belongingType)) -> 
                (Right (KeliTypeCheckedExpr (KeliTagConstructor tag Nothing) belongingType))

            Just (KeliSymTag (KeliTagCarryful tag carryType belongingType)) ->
                -- How to check if user forgot to put .carry ?
                --  Don't need to explicitly check it, the type system will handle it
                (Right (KeliTypeCheckedExpr expr (KeliTypeCarryfulTagConstructor tag carryType belongingType)))
            
            Just (KeliSymType (KeliTypeAlias _ (KeliTypeRecord propTypePairs))) -> 
                (Right (KeliTypeCheckedExpr (KeliRecordConstructor propTypePairs) (KeliTypeRecordConstructor propTypePairs)))

            Just (KeliSymType t@(KeliTypeParam{})) ->
                error "shouldn't reach here, preprocessDecl should already converted those things"
            
            Nothing -> 
                Left (KErrorUsingUndefinedId token)
            
            other -> 
                undefined


    KeliFuncCall params funcIds ref -> do
        typeCheckedParams <- typeCheckExprs symtab params
        typeCheckFuncCall symtab typeCheckedParams funcIds

    KeliRecord kvs (Just propTypePairs) -> do
        let actualProps = map fst kvs 
        values <- typeCheckExprs symtab (map snd kvs)
        let expectedPropTypePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) propTypePairs 
        let actualPropValuePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) (zip actualProps values) 
        case find (\(expected, actual) -> 
                let expectedType = snd expected in
                let actualType = getType (snd actual) in
                not (expectedType `typeEquals` actualType)
            ) (zip expectedPropTypePairs actualPropValuePairs) of
            Just (expected, actual) -> 
                Left (KErrorPropretyTypeMismatch (fst expected) (snd expected) (snd actual))
            Nothing -> 
                Right (KeliTypeCheckedExpr (KeliRecord actualPropValuePairs (Just propTypePairs)) (KeliTypeRecord propTypePairs))

    KeliRecord kvs Nothing -> do
        let keys = map fst kvs 
        let values = map snd kvs 
        typeCheckedExprs <- typeCheckExprs symtab values
        Right 
            (KeliTypeCheckedExpr 
                (KeliRecord (zip keys values) Nothing) 
                (KeliTypeRecord (zip keys (map getType typeCheckedExprs))))
    
    KeliTagConstructor id carry -> 
        case carry of 
            Just expr -> do
                typeCheckedCarryExpr <- typeCheckExpr symtab expr
                case lookup (snd id) symtab of
                    Just (KeliSymTag (KeliTagCarryful _ expectedCarryType belongingType)) ->
                        if getType typeCheckedCarryExpr `typeEquals` expectedCarryType then
                            return (KeliTypeCheckedExpr (KeliTagConstructor id (Just typeCheckedCarryExpr)) belongingType)
                        else 
                            Left (KErrorIncorrectCarryType expectedCarryType typeCheckedCarryExpr)
                    other -> 
                        error "should'nt reach here, if so there might be problems with preprocessExpr"

            Nothing ->
                undefined

    r@(KeliRecordSetter subject propertyName value expectedPropertyType recordType) -> do
        typeCheckedValue <- typeCheckExpr symtab value
        if getType typeCheckedValue `typeEquals` expectedPropertyType then
            return (KeliTypeCheckedExpr (r {recordSetterNewValue = typeCheckedValue}) recordType)
        else
            Left (KErrorWrongTypeInSetter)

    KeliRecordConstructor kvs ->
        return (KeliTypeCheckedExpr e (KeliTypeRecordConstructor kvs))

    KeliTagMatcher subject branches elseBranch -> do
        -- check if all branch have the same type
        allBranchesExpr <- mapM (typeCheckExpr symtab) (map snd branches ++ (case elseBranch of Just x -> [x]; _ -> [])) 
        let firstBranch = head allBranchesExpr 
        if any (\x -> not (getType x `typeEquals` getType firstBranch)) allBranchesExpr then
            Left (KErrorNotAllBranchHaveTheSameType allBranchesExpr)
        else
            -- complete tag matches
            case elseBranch of 
                Just x -> 
                    Right (KeliTypeCheckedExpr (KeliTagMatcher subject branches (Just x)) (getType (head allBranchesExpr)))

                Nothing ->
                    Right (KeliTypeCheckedExpr (KeliTagMatcher subject branches Nothing) (getType (head allBranchesExpr)))

    other -> 
        undefined

    where 

-- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
typeCheckFuncCall :: KeliSymTab -> [KeliExpr] -> [StringToken] -> Either KeliError KeliExpr
typeCheckFuncCall symtab funcCallParams funcIds = 
    let funcId = intercalate "$" (map snd funcIds) in
    let actualParamTypes = map (getType) funcCallParams in
    case lookup funcId symtab of
        Just (KeliSymFunc candidateFuncs) -> do
            case (find 
                    (\f ->
                        if length funcCallParams /= length (funcDeclParams f) then
                            False
                        else
                            let expectedParamTypes = map (\(_,paramType) -> paramType) (funcDeclParams f) in
                            all 
                                (\(actualType, expectedType) -> actualType `haveShapeOf` expectedType) 
                                (zip actualParamTypes expectedParamTypes))

                    -- This sorting is necessary so that the compiler will look for more specialized (a.k.a less generic) function first
                    (sortOn (\f -> length (funcDeclGenericParams f)) candidateFuncs)) of

                Just matchingFunc -> do
                    let genericParams = funcDeclGenericParams matchingFunc
                    -- 1. Create empty binding table
                    let initialBindingTable = (fromList (map (\((_,id),_) -> (id, Nothing)) genericParams)) :: GenericBindingTable

                    -- 1.1 Populate binding table
                    let expectedParamTypes = map snd (funcDeclParams matchingFunc)

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
                                                            error "shouldn't be possible") :: GenericBindingTable -> (StringToken, KeliType) -> GenericBindingTable)

                                                (bindingTable2 :: GenericBindingTable)
                                                bindings)

                                        Left err -> Left err
                                    ) :: GenericBindingTable -> (KeliType, KeliType) -> Either KeliError GenericBindingTable)
                                initialBindingTable
                                (zip expectedParamTypes actualParamTypes)))

                    
                    -- 2. Subsitute type param bindings
                    let specializedFunc = substituteGeneric populatedBindingTable matchingFunc 

                    -- 3. Check if each func call param type match with param types of specializedFunc
                    let typeMismatchError = 
                            find
                            (\(expectedType, actualExpr) -> not (getType actualExpr `typeEquals` expectedType))
                            (zip (map snd (funcDeclParams specializedFunc)) funcCallParams)
                    
                    case typeMismatchError of
                        Just (expectedType, actualExpr) -> 
                            Left (KErrorFuncCallTypeMismatch expectedType actualExpr)

                        Nothing ->
                            let funcCall = KeliFuncCall funcCallParams funcIds (Just matchingFunc) in
                            Right (KeliTypeCheckedExpr funcCall (funcDeclReturnType specializedFunc))

                Nothing ->
                    Left (KErrorUsingUndefinedFunc funcIds candidateFuncs)
        
        Just other ->
            Left (KErrorNotAFunction other)

        _ -> 
            Left (KErrorUsingUndefinedFunc funcIds [])

substituteGeneric :: GenericBindingTable -> KeliFunc -> KeliFunc
substituteGeneric bindingTable matchingFunc = 
    let expectedFuncParams = funcDeclParams matchingFunc in
    let substitutedFuncParams = 
            map (\(paramId, paramType) -> (paramId, substituteGeneric' bindingTable paramType)) expectedFuncParams in

    let substitutedReturnType = substituteGeneric' bindingTable (funcDeclReturnType matchingFunc) in

    (matchingFunc {
        funcDeclParams = substitutedFuncParams, 
        funcDeclReturnType = substitutedReturnType})
    
substituteGeneric' :: GenericBindingTable -> KeliType -> KeliType
substituteGeneric' bindingTable type' =
    case type' of
        KeliTypeParam (_,id) _ -> 
            case lookup id bindingTable of 
                Just (Just bindingType) -> bindingType
                _ -> error "possibly due to binding table is not populated properly"
        
        KeliTypeCompound _ _ -> 
            undefined

        _ -> 
            type'


type TypeVerifier = KeliSymTab -> KeliType -> Either KeliError KeliType

verifyType :: TypeVerifier
verifyType symtab type' =
    case type' of
        KeliTypeUnverified expr ->
            case expr of
                (KeliId token@(_,id)) -> 
                    case lookup id symtab of
                        Just (KeliSymType constraint@(KeliTypeConstraint{})) -> Right constraint
                        Just (KeliSymType t)                                 -> Right t
                        Just other                                           -> Left (KErrorExprIsNotAType expr) 
                        Nothing                                              -> Left (KErrorUsingUndefinedId token)

        t -> 
            Right t

verifyTypeConstraint :: TypeVerifier
verifyTypeConstraint symtab type' = 
    case type' of 
        KeliTypeAlias name aliasingType ->
            let key = intercalate "$" (map snd name) in
            case lookup key symtab of
                Just (KeliSymType (KeliTypeAlias _ constraint@(KeliTypeConstraint{}))) -> Right constraint
                Just other -> Left (KErrorNotATypeConstraint other)
                Nothing    -> Left (KErrorUsingUndefinedType name)
        
        _ -> 
            undefined

typeCheckExprs :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliExpr]
typeCheckExprs symtab exprs = mapM (typeCheckExpr symtab) exprs


getTypeWithoutResolvingAlias :: KeliExpr -> KeliType
getTypeWithoutResolvingAlias e = case e of
    KeliTypeCheckedExpr _ t -> t
    _ -> undefined

    
-- Example
--  a.list `haveShapeOf` b.list = True
--  a.list.list `haveShapeOf` b.list.list = True
--  a.list `haveShapeOf` a.tree = False
--  int `haveShapeOf` int = True
--  int `haveShapeOf` a = True
haveShapeOf :: KeliType -> KeliType -> Bool
type1 `haveShapeOf` type2 = 
    case type1 of
        KeliTypeCompound _ _ -> 
            undefined
        
        _ -> 
            case type2 of
                KeliTypeParam _ _ -> 
                    True
                
                _ ->
                    type1 `typeEquals` type2

hardConformsTo :: KeliType -> KeliConstraint -> Bool
type' `hardConformsTo` constraint =
    case constraint of
        KeliConstraintAny -> 
            True
        _ -> 
            undefined

data GenericParamLocation 
    = GenericParamNotFound
    | GenericParamFoundAsSimpleType
        StringToken-- generic param name
        KeliConstraint

    | GenericParamFoundAsCompoundType
        [(
            Int, -- generic param index,
            GenericParamLocation
        )]
    deriving (Show)

whereAreGenericParams :: KeliType -> GenericParamLocation
whereAreGenericParams t = case t of
    -- compound type 
    KeliTypeCompound _ _ -> undefined

    -- simple type
    KeliTypeParam id constraint -> GenericParamFoundAsSimpleType id constraint

    _ -> GenericParamNotFound

data CorrespondingType
    = CorrespondingTypeNotFound
    | CorrespondingTypeFound [(StringToken, KeliType)]
    deriving(Show)

findCorrespondingType :: GenericParamLocation -> KeliType -> Either KeliError CorrespondingType
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

type GenericBindingTable = OMap String (Maybe KeliType) 