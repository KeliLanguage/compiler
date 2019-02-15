module Unify where 

import qualified Ast.Verified as V
import Util
import Symbol
import StaticError
import Prelude hiding(lookup)
import Data.List hiding(lookup)
import Data.Map.Ordered ((|>), lookup, member) 


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