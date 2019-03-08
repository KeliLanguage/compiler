module Unify where 

import Control.Monad

import qualified Ast.Verified as V
import Util
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import StaticError
import Prelude hiding(lookup)
import qualified Data.Map.Strict as Map

getType :: V.Expr -> V.Type
getType (V.Expr _ t) = t

type Substitution = Map.Map String V.Type

emptySubstitution :: Map.Map String V.Type
emptySubstitution = Map.empty

type UnifyResult = Either KeliError Substitution


unifyMany 
    :: Substitution -- initial subst
    -> [V.Expr]  -- actual expr (for reporting error location only)
    -> [V.Type]  -- expected type
    -> UnifyResult

unifyMany initialSubst exprs expectedTypes = 
    if length exprs /= length expectedTypes then
        error ("Length of exprs should be the same as length of expectedTypes")
    else
        foldM 
            (\prevSubst (actualExpr, expectedType) -> do
                -- apply previous substituion to current expectedParamType
                let expectedType' = applySubstitutionToType prevSubst expectedType
                nextSubst <- unify actualExpr expectedType'
                Right (composeSubst prevSubst nextSubst))
            initialSubst
            (zip exprs expectedTypes)

unify :: 
       V.Expr  -- actual expr (for reporting error location only)
    -> V.Type  -- expected type
    -> UnifyResult

unify (V.Expr actualExpr actualType) expectedType =
    unify' actualExpr actualType expectedType

unify' :: 
       V.Expr' -- actual expr (for reporting error location only)
    -> V.Type  -- actual type
    -> V.Type  -- expected type
    -> UnifyResult

-- unify' type variables
unify' actualExpr (V.FreeTypeVar name constraint) t =
    unifyTVar actualExpr name constraint t

unify' actualExpr t (V.FreeTypeVar name constraint) =
    unifyTVar actualExpr name constraint t

-- unify' named types
unify' _ (V.TypeFloat) (V.TypeFloat) = 
    Right (emptySubstitution)  

unify' _ (V.TypeInt) (V.TypeInt) = 
    Right (emptySubstitution)  

unify' _ (V.TypeString) (V.TypeString) = 
    Right (emptySubstitution)  

-- unify' bounded type variables
unify' 
    actualExpr 
    actualType@(V.BoundedTypeVar name1 _) 
    expectedType@(V.BoundedTypeVar name2 _) = 
    if snd name1 == snd name2 then
        Right (emptySubstitution)  
    else
        Left (KErrorTypeMismatch actualExpr actualType expectedType)

-- unify' carryful tag counstructor
unify' 
    actualExpr 
    actualType@(V.TypeCarryfulTagConstructor x _ _ _)
    expectedType@(V.TypeCarryfulTagConstructor y _ _ _) = 
    if x == y then 
        Right (emptySubstitution)  
    else 
        Left (KErrorTypeMismatch actualExpr actualType expectedType)

unify' 
    actualExpr
    actualType@(V.TypeObjectConstructor name1 _)
    expectedType@(V.TypeObjectConstructor name2 _) = 
    if name1 == name2 then
        Right (emptySubstitution)
    else
        Left (KErrorTypeMismatch actualExpr actualType expectedType)

unify' _ V.TypeType V.TypeType = 
    undefined

-- unify' tagged union
unify'
    actualExpr
    actualType@(V.TypeTaggedUnion (V.TaggedUnion name1 _ _ actualInnerTypes))
    expectedType@(V.TypeTaggedUnion (V.TaggedUnion name2 _ _ expectedInnerTypes)) = 
    if name1 == name2 && (length actualInnerTypes == length expectedInnerTypes) then do
        foldM 
            (\prevSubst (actualInnerType, expectedInnerType) -> do
                nextSubst <- unify' actualExpr actualInnerType expectedInnerType
                Right (composeSubst prevSubst nextSubst))
            emptySubstitution
            (zip actualInnerTypes expectedInnerTypes)
    else 
        Left (KErrorTypeMismatch actualExpr actualType expectedType)


-- unfify object type
-- object type is handled differently, because we want to have structural typing
-- NOTE: kts means "key-type pairs"
unify' actualExpr (V.TypeObject _ kts1) (V.TypeObject _ kts2) = 
    let (actualKeys, actualTypes) = unzip kts1 in
    let (expectedKeys, expectedTypes) = unzip kts2 in
    -- TODO: get the set difference of expectedKeys with actualKeys
    -- because we want to do structural typing
    -- that means, it is acceptable if actualKeys is a valid subset of expectedKeys
    case match actualKeys expectedKeys of
        PerfectMatch ->
            foldM
                (\prevSubst (key, actualType, expectedType) -> 
                    case unify' actualExpr actualType (applySubstitutionToType prevSubst expectedType) of
                        Right nextSubst ->
                            Right (composeSubst prevSubst nextSubst)

                        Left KErrorTypeMismatch{} ->
                            Left (KErrorPropertyTypeMismatch key expectedType actualType actualExpr )

                        Left err ->
                            Left err)
                emptySubstitution
                (zip3 actualKeys actualTypes expectedTypes)

        GotDuplicates duplicates ->
            Left (KErrorDuplicatedProperties duplicates)

        GotExcessive excessiveProps ->
            Left (KErrorExcessiveProperties excessiveProps)
        
        Missing missingProps ->
            Left (KErrorMissingProperties actualExpr missingProps)

        ZeroIntersection ->
            Left (KErrorMissingProperties actualExpr (map snd expectedKeys))
        

unify' actualExpr actualType expectedType =  Left (KErrorTypeMismatch actualExpr actualType expectedType)


unifyTVar :: V.Expr' -> String -> Maybe V.TypeConstraint -> V.Type -> UnifyResult
unifyTVar actualExpr tvarname1 _ t2 =
    -- NOTE: actualExpr is used for reporting error location only
    let result = Right (Map.insert tvarname1 t2 emptySubstitution) in
    case t2 of
        V.FreeTypeVar tvarname2 _ ->
            if tvarname1 == tvarname2 then
                Right emptySubstitution
            else
                result

        _ ->
            if t2 `contains` tvarname1 then
                Left (KErrorTVarSelfReferencing actualExpr tvarname1 t2)
            else
                result 


contains :: V.Type -> String -> Bool
t `contains` tvarname = 
    case t of
        V.BoundedTypeVar (_,name) _ ->
            name == tvarname

        V.FreeTypeVar name _ ->
            name == tvarname

        V.TypeTaggedUnion (V.TaggedUnion _ _ _ types) ->
            any (`contains` tvarname) types

        _ ->
            False

{- 
    Composing substitution s1 and s1

     For example if 

        s1 = {t1 => Int, t3 => t2} 
        s2 = {t2 => t1}

    Then the result will be

        s3 = {
            t1 => Int,
            t2 => Int,
            t3 => Int
        }
-}
composeSubst :: Substitution -> Substitution -> Substitution 
composeSubst s1 s2 =
    let result = 
            foldl 
                (\subst (key, type') -> Map.insert key (applySubstitutionToType s1 type') subst) 
                emptySubstitution
                ((Map.assocs s2)::[(String, V.Type)]) in

    -- cannot be Map.union result s1
    -- because we want keys in result to override duplicates found in s1
    Map.union result s1


-- Replace the type variables in a type that are
-- present in the given substitution and return the
-- type with those variables with their substituted values
-- eg. Applying the substitution {"a": Bool, "b": Int}
-- to a type (a -> b) will give type (Bool -> Int)
applySubstitutionToType :: Substitution -> V.Type -> V.Type
applySubstitutionToType subst type' =
    case type' of
        V.FreeTypeVar name _ ->
            case Map.lookup name subst of
                Just t ->
                    t
                Nothing ->
                    type'

        V.BoundedTypeVar name _ ->
            case Map.lookup (snd name) subst of
                Just t ->
                    t
                Nothing ->
                    type'

        V.TypeTaggedUnion (V.TaggedUnion name ids tags innerTypes) ->
            V.TypeTaggedUnion (V.TaggedUnion name ids tags (map (applySubstitutionToType subst) innerTypes))

        V.TypeObject name propTypePairs ->
            let (props, types) = unzip propTypePairs in
            V.TypeObject name (zip props (map (applySubstitutionToType subst) types))
        
        other ->
            other