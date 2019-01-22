module TypeCheck where

import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered (OMap, (|>), assocs, member, lookup, empty, fromList) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)
import Data.Maybe (catMaybes, fromJust)

import qualified Ast.Raw as Raw
import qualified Ast.Verified
import StaticError
import Symbol
import Util

typeCheckExpr :: KeliSymTab -> Raw.Expr -> Either KeliError Raw.Expr
typeCheckExpr symtab expr = 
    case typeCheckExpr' symtab expr of 
        Right x@Raw.TypeCheckedExpr{} -> 
            Right x

        Right _ -> 
            error ("return type of typeCheckExpr' should be Raw.TypeCheckedExpr but received " ++ show expr)

        Left err -> 
            Left err


typeCheckExpr' :: KeliSymTab -> Raw.Expr -> Either KeliError Raw.Expr
typeCheckExpr' symtab e = case e of 
    (expr@(Raw.Number(_,n))) -> 
        (Right(Raw.TypeCheckedExpr expr (case n of Left _ -> Raw.TypeInt; Right _ -> Raw.TypeFloat)))

    (expr@(Raw.String _)) -> 
        (Right(Raw.TypeCheckedExpr expr Raw.TypeString))

    (expr@(Raw.TypeCheckedExpr _ _)) -> 
        (Right expr)

    (expr@(Raw.Id token@(_,id))) -> 
        case lookup id symtab of 
            Just (KeliSymConst (Raw.Const _ (Raw.TypeCheckedExpr _ exprType) _)) -> 
                (Right (Raw.TypeCheckedExpr expr exprType))
        
            Just (KeliSymSingleton token') -> 
                (Right (Raw.TypeCheckedExpr expr (Raw.TypeSingleton token')))

            Just (KeliSymTag (Raw.TagCarryless tag belongingType)) -> 
                (Right (Raw.TypeCheckedExpr (Raw.TagConstructor tag Nothing) belongingType))

            Just (KeliSymTag (Raw.TagCarryful tag carryType belongingType)) ->
                -- How to check if user forgot to put .carry ?
                --  Don't need to explicitly check it, the type system will handle it
                (Right (Raw.TypeCheckedExpr expr (Raw.TypeCarryfulTagConstructor tag carryType belongingType)))
            
            Just (KeliSymType (Raw.TypeAlias _ (Raw.TypeRecord propTypePairs))) -> 
                (Right (Raw.TypeCheckedExpr (Raw.RecordConstructor propTypePairs) (Raw.TypeRecordConstructor propTypePairs)))

            Just (KeliSymType t@(Raw.TypeParam{})) ->
                error "shouldn't reach here, preprocessDecl should already converted those things"
            
            Nothing -> 
                Left (KErrorUsingUndefinedId token)
            
            other -> 
                undefined


    Raw.FuncCall params funcIds ref -> do
        typeCheckedParams <- typeCheckExprs symtab params
        typeCheckFuncCall symtab typeCheckedParams funcIds

    Raw.Record kvs (Just propTypePairs) -> do
        let actualProps = map fst kvs 
        values <- typeCheckExprs symtab (map snd kvs)
        let expectedPropTypePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) propTypePairs 
        let actualPropValuePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) (zip actualProps values) 
        case find (\(expected, actual) -> 
                let expectedType = snd expected in
                let actualType = getType (snd actual) in
                not (expectedType `Raw.typeEquals` actualType)
            ) (zip expectedPropTypePairs actualPropValuePairs) of
            Just (expected, actual) -> 
                Left (KErrorPropretyTypeMismatch (fst expected) (snd expected) (snd actual))
            Nothing -> 
                Right (Raw.TypeCheckedExpr (Raw.Record actualPropValuePairs (Just propTypePairs)) (Raw.TypeRecord propTypePairs))

    Raw.Record kvs Nothing -> do
        let keys = map fst kvs 
        let values = map snd kvs 
        typeCheckedExprs <- typeCheckExprs symtab values
        Right 
            (Raw.TypeCheckedExpr 
                (Raw.Record (zip keys values) Nothing) 
                (Raw.TypeRecord (zip keys (map getType typeCheckedExprs))))
    
    Raw.TagConstructor id carry -> 
        case carry of 
            Just expr -> do
                typeCheckedCarryExpr <- typeCheckExpr symtab expr
                case lookup (snd id) symtab of
                    Just (KeliSymTag (Raw.TagCarryful _ expectedCarryType belongingType)) ->
                        if getType typeCheckedCarryExpr `Raw.typeEquals` expectedCarryType then
                            return (Raw.TypeCheckedExpr (Raw.TagConstructor id (Just typeCheckedCarryExpr)) belongingType)
                        else 
                            Left (KErrorIncorrectCarryType expectedCarryType typeCheckedCarryExpr)
                    other -> 
                        error "should'nt reach here, if so there might be problems with preprocessExpr"

            Nothing ->
                undefined

    r@(Raw.RecordSetter subject propertyName value expectedPropertyType recordType) -> do
        typeCheckedValue <- typeCheckExpr symtab value
        if getType typeCheckedValue `Raw.typeEquals` expectedPropertyType then
            return (Raw.TypeCheckedExpr (r {Raw.recordSetterNewValue = typeCheckedValue}) recordType)
        else
            Left (KErrorWrongTypeInSetter)

    Raw.RecordConstructor kvs ->
        return (Raw.TypeCheckedExpr e (Raw.TypeRecordConstructor kvs))

    Raw.TagMatcher subject branches elseBranch -> do
        -- check if all branch have the same type
        allBranchesExpr <- mapM (typeCheckExpr symtab) (map snd branches ++ (case elseBranch of Just x -> [x]; _ -> [])) 
        let firstBranch = head allBranchesExpr 
        if any (\x -> not (getType x `Raw.typeEquals` getType firstBranch)) allBranchesExpr then
            Left (KErrorNotAllBranchHaveTheSameType allBranchesExpr)
        else
            -- complete tag matches
            case elseBranch of 
                Just x -> 
                    Right (Raw.TypeCheckedExpr (Raw.TagMatcher subject branches (Just x)) (getType (head allBranchesExpr)))

                Nothing ->
                    Right (Raw.TypeCheckedExpr (Raw.TagMatcher subject branches Nothing) (getType (head allBranchesExpr)))

    other -> 
        undefined

    where 

-- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
typeCheckFuncCall :: KeliSymTab -> [Raw.Expr] -> [Raw.StringToken] -> Either KeliError Raw.Expr
typeCheckFuncCall symtab funcCallParams funcIds = 
    let funcId = intercalate "$" (map snd funcIds) in
    let actualParamTypes = map (getType) funcCallParams in
    case lookup funcId symtab of
        Just (KeliSymFunc candidateFuncs) -> do
            case (find 
                    (\f ->
                        if length funcCallParams /= length (Raw.funcDeclParams f) then
                            False
                        else
                            let expectedParamTypes = map (\(_,paramType) -> paramType) (Raw.funcDeclParams f) in
                            all 
                                (\(actualType, expectedType) -> actualType `haveShapeOf` expectedType) 
                                (zip actualParamTypes expectedParamTypes))

                    -- This sorting is necessary so that the compiler will look for more specialized (a.k.a less generic) function first
                    (sortOn (\f -> length (Raw.funcDeclGenericParams f)) candidateFuncs)) of

                Just matchingFunc -> do
                    let genericParams = Raw.funcDeclGenericParams matchingFunc
                    -- 1. Create empty binding table
                    let initialBindingTable = (fromList (map (\((_,id),_) -> (id, Nothing)) genericParams)) :: GenericBindingTable

                    -- 1.1 Populate binding table
                    let expectedParamTypes = map snd (Raw.funcDeclParams matchingFunc)

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
                                                            error "shouldn't be possible") :: GenericBindingTable -> (Raw.StringToken, Raw.Type) -> GenericBindingTable)

                                                (bindingTable2 :: GenericBindingTable)
                                                bindings)

                                        Left err -> Left err
                                    ) :: GenericBindingTable -> (Raw.Type, Raw.Type) -> Either KeliError GenericBindingTable)
                                initialBindingTable
                                (zip expectedParamTypes actualParamTypes)))

                    
                    -- 2. Subsitute type param bindings
                    let specializedFunc = substituteGeneric populatedBindingTable matchingFunc 

                    -- 3. Check if each func call param type match with param types of specializedFunc
                    let typeMismatchError = 
                            find
                            (\(expectedType, actualExpr) -> not (getType actualExpr `Raw.typeEquals` expectedType))
                            (zip (map snd (Raw.funcDeclParams specializedFunc)) funcCallParams)
                    
                    case typeMismatchError of
                        Just (expectedType, actualExpr) -> 
                            Left (KErrorFuncCallTypeMismatch expectedType actualExpr)

                        Nothing ->
                            let funcCall = Raw.FuncCall funcCallParams funcIds (Just matchingFunc) in
                            Right (Raw.TypeCheckedExpr funcCall (Raw.funcDeclReturnType specializedFunc))

                Nothing ->
                    Left (KErrorUsingUndefinedFunc funcIds candidateFuncs)
        
        Just other ->
            Left (KErrorNotAFunction other)

        _ -> 
            Left (KErrorUsingUndefinedFunc funcIds [])

substituteGeneric :: GenericBindingTable -> Raw.Func -> Raw.Func
substituteGeneric bindingTable matchingFunc = 
    let expectedFuncParams = Raw.funcDeclParams matchingFunc in
    let substitutedFuncParams = 
            map (\(paramId, paramType) -> (paramId, substituteGeneric' bindingTable paramType)) expectedFuncParams in

    let substitutedReturnType = substituteGeneric' bindingTable (Raw.funcDeclReturnType matchingFunc) in

    (matchingFunc {
        Raw.funcDeclParams = substitutedFuncParams, 
        Raw.funcDeclReturnType = substitutedReturnType})
    
substituteGeneric' :: GenericBindingTable -> Raw.Type -> Raw.Type
substituteGeneric' bindingTable type' =
    case type' of
        Raw.TypeParam (_,id) _ -> 
            case lookup id bindingTable of 
                Just (Just bindingType) -> bindingType
                _ -> error "possibly due to binding table is not populated properly"
        
        Raw.TypeCompound _ _ -> 
            undefined

        _ -> 
            type'


type TypeVerifier = KeliSymTab -> Raw.Type -> Either KeliError Raw.Type

verifyType :: TypeVerifier
verifyType symtab type' =
    case type' of
        Raw.TypeUnverified expr ->
            case expr of
                (Raw.Id token@(_,id)) -> 
                    case lookup id symtab of
                        Just (KeliSymType constraint@(Raw.TypeConstraint{})) -> Right constraint
                        Just (KeliSymType t)                                 -> Right t
                        Just other                                           -> Left (KErrorExprIsNotAType expr) 
                        Nothing                                              -> Left (KErrorUsingUndefinedId token)

        t -> 
            Right t

verifyTypeConstraint :: TypeVerifier
verifyTypeConstraint symtab type' = 
    case type' of 
        Raw.TypeAlias name aliasingType ->
            let key = intercalate "$" (map snd name) in
            case lookup key symtab of
                Just (KeliSymType (Raw.TypeAlias _ constraint@(Raw.TypeConstraint{}))) -> Right constraint
                Just other -> Left (KErrorNotATypeConstraint other)
                Nothing    -> Left (KErrorUsingUndefinedType name)
        
        _ -> 
            undefined

typeCheckExprs :: KeliSymTab -> [Raw.Expr] -> Either KeliError [Raw.Expr]
typeCheckExprs symtab exprs = mapM (typeCheckExpr symtab) exprs


getTypeWithoutResolvingAlias :: Raw.Expr -> Raw.Type
getTypeWithoutResolvingAlias e = case e of
    Raw.TypeCheckedExpr _ t -> t
    _ -> undefined

    
-- Example
--  a.list `haveShapeOf` b.list = True
--  a.list.list `haveShapeOf` b.list.list = True
--  a.list `haveShapeOf` a.tree = False
--  int `haveShapeOf` int = True
--  int `haveShapeOf` a = True
haveShapeOf :: Raw.Type -> Raw.Type -> Bool
type1 `haveShapeOf` type2 = 
    case type1 of
        Raw.TypeCompound _ _ -> 
            undefined
        
        _ -> 
            case type2 of
                Raw.TypeParam _ _ -> 
                    True
                
                _ ->
                    type1 `Raw.typeEquals` type2

hardConformsTo :: Raw.Type -> Raw.Constraint -> Bool
type' `hardConformsTo` constraint =
    case constraint of
        Raw.ConstraintAny -> 
            True
        _ -> 
            undefined

data GenericParamLocation 
    = GenericParamNotFound
    | GenericParamFoundAsSimpleType
        Raw.StringToken-- generic param name
        Raw.Constraint

    | GenericParamFoundAsCompoundType
        [(
            Int, -- generic param index,
            GenericParamLocation
        )]
    deriving (Show)

whereAreGenericParams :: Raw.Type -> GenericParamLocation
whereAreGenericParams t = case t of
    -- compound type 
    Raw.TypeCompound _ _ -> undefined

    -- simple type
    Raw.TypeParam id constraint -> GenericParamFoundAsSimpleType id constraint

    _ -> GenericParamNotFound

data CorrespondingType
    = CorrespondingTypeNotFound
    | CorrespondingTypeFound [(Raw.StringToken, Raw.Type)]
    deriving(Show)

findCorrespondingType :: GenericParamLocation -> Raw.Type -> Either KeliError CorrespondingType
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

type GenericBindingTable = OMap String (Maybe Raw.Type) 