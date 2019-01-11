{-# LANGUAGE MultiParamTypeClasses #-}
module Analyzer where

import Ast
import Prelude hiding (lookup)
import SymbolTable
import Data.Map.Ordered ((|>), assocs, member, lookup)
import Data.List hiding (lookup)
import Text.Parsec.Pos
import StaticError
import Debug.Trace
import Control.Monad
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

analyze :: KeliSymTab -> Either KeliError [KeliSym]
analyze symtab = do
    let symbols       = map snd (assocs symtab)
    let constSymbols  = filter (\x -> case x of KeliSymConst _ -> True; _->False) symbols 
    symtab'          <- analyzeSymbols constSymbols symtab
    let funcSymbols   = filter (\x -> case x of KeliSymFunc _ -> True; _->False) symbols 
    finalSymtab      <- analyzeSymbols funcSymbols symtab'
    let symbols       = map snd (assocs finalSymtab)
    -- sorting is necessary, so that the transpilation order will be correct
    -- Smaller number means will be transpiled first
    let sortedSymbols = sortOn (
            \x -> case x of 
                KeliSymSingleton _ -> 0
                KeliSymTag _       -> 1
                KeliSymFunc _      -> 2
                KeliSymConst _     -> 3
                KeliSymType _      -> 4
            ) symbols
    return sortedSymbols 


analyzeSymbols :: [KeliSym] -> KeliSymTab -> Either KeliError KeliSymTab
analyzeSymbols symbols symtab0 = 
    foldM
    ((\symtab1 nextSym1 -> do
        (analyzedSymbol, extraSymbols) <- analyzeSymbol nextSym1 symtab1

        -- insert the const symbol into symtab
        let (_,key) = getIdentifier nextSym1
        let symtab2 = symtab1 |> (key, analyzedSymbol)

        -- insert extra symbols into symtab (i.e., tags)
        let symtab3 = 
                foldM
                ((\symtab4 nextSym2 -> 
                        let token@(_,key') = getIdentifier nextSym2 in
                        if member key' symtab4 then
                            Left (KErrorDuplicatedId token)
                        else
                            Right (symtab4 |> (key', nextSym2))
                    ))
                symtab2
                extraSymbols

        symtab3
    )::KeliSymTab -> KeliSym -> Either KeliError KeliSymTab)
    symtab0
    symbols

analyzeSymbol :: KeliSym -> KeliSymTab -> Either KeliError (KeliSym,[KeliSym])
analyzeSymbol symbol symtab = case symbol of
    KeliSymConst KeliConst {
        constDeclId=id,
        constDeclValue=expr,
        constDeclType=expectedType
    } -> do 
            checkedExpr <- typeCheckExpr symtab expr
            case checkedExpr of
                KeliTypeExpr t 
                    -> return (KeliSymType t, [])

                KeliCarrylessTagExpr tag
                    -> 
                    let unionType = KeliTypeTagUnion [tag] in
                    return ( KeliSymType unionType, [ KeliSymTag (KeliTagCarryless tag unionType) ])
                
                KeliCarryfulTagExpr _ _
                    -> undefined
                
                KeliTagUnionExpr tags
                    -> 
                    let extractTags x = case x of KeliTagCarryless t _ -> t; KeliTagCarryful t _ _ -> t; in
                    let unionType = KeliTypeTagUnion (map extractTags tags) in
                    -- assign belonging types to each tags
                    -- because initially their belonging type is assigned as KeliTypeUndefined
                    let tags' = map ( \x -> case x of 
                                    KeliTagCarryless t _          -> KeliTagCarryless t unionType; 
                                    KeliTagCarryful t carryType _ -> KeliTagCarryful t carryType unionType; 
                                ) tags in
                    return (KeliSymType unionType, map KeliSymTag tags')
                _              
                    -> return (KeliSymConst (KeliConst id checkedExpr expectedType),[])

    KeliSymFunc (func@KeliFunc {
        funcDeclParams=params,
        funcDeclReturnType=returnType,
        funcDeclBody=body
    }) -> do 
            -- 1. populate symbol table with function parameters
            symtab' <- 
                foldM 
                ((\acc (KeliFuncDeclParam id expectedType) -> 
                    let id' = snd id in
                    if member id' acc 
                        then Left (KErrorDuplicatedId id)
                        else Right(
                            let keyValue = (id', (KeliSymConst (KeliConst id (KeliTypeCheckedExpr (KeliId id) expectedType) Nothing)))
                            in acc |> keyValue
                                ))::KeliSymTab -> KeliFuncDeclParam -> Either KeliError KeliSymTab)
                symtab
                params
            
            -- 2. type check the function body
            typeCheckedBody <- typeCheckExpr symtab' body

            -- 3. ensure body type adheres to return type
            if getType typeCheckedBody == returnType then
                Right (KeliSymFunc (func {funcDeclBody = typeCheckedBody}),[])
            else
                Left KErrorUnmatchingFuncReturnType

    
typeCheckExpr :: KeliSymTab -> KeliExpr  -> Either KeliError KeliExpr
typeCheckExpr symtab e = case e of 
    (expr@(KeliNumber(_,n))) 
        -> Right (KeliTypeCheckedExpr expr (case n of Left _ -> KeliTypeInt; Right _ -> KeliTypeFloat))

    (expr@(KeliString _))    
        -> Right (KeliTypeCheckedExpr expr KeliTypeString)

    (expr@(KeliTypeCheckedExpr _ _)) 
        -> Right expr

    (expr@(KeliId token@(_,id)))
        -> 
        (case lookup id symtab of 
            Just (KeliSymConst (KeliConst _ (KeliTypeCheckedExpr _ exprType) _))
                -> Right (KeliTypeCheckedExpr expr exprType)
        
            Just (KeliSymSingleton token)
                -> Right (KeliTypeCheckedExpr expr (KeliTypeSingleton token))

            Just (KeliSymTag (KeliTagCarryless tag type'))
                -> Right (KeliTypeCheckedExpr expr type')

            Just (KeliSymTag (KeliTagCarryful _ _ _))
                -> undefined
            _ 
                -> Left (KErrorUsingUndefinedId token))

    -- NOTES:
    -- * Why isn't func call params type checked at the very beginning?
    --      Because we need to cater for the magic `_.tag XXX` and `record.XXX YYY` function
    (expr@(KeliFuncCall params ids))
        -> 
        case head ids of 
        -- 0. Check if user wants to create a union
        (_,"or")
            -> do
                typeCheckedParams <- typeCheckExprs symtab params
                let isTagOrUnion x = case x of KeliCarrylessTagExpr _  -> True; KeliCarryfulTagExpr _ _ -> True; KeliTagUnionExpr _ -> True; _ -> False;
                if isTagOrUnion (head typeCheckedParams) then (
                    if length typeCheckedParams /= 2 then
                        Left KErrorIncorrectUsageOfTaggedUnion
                    else if any (not . isTagOrUnion) typeCheckedParams then
                        Left KErrorIncorrectUsageOfTaggedUnion
                    else
                        let extractTags x = case x of
                                KeliCarrylessTagExpr tag   -> Right [KeliTagCarryless tag KeliTypeUndefined]
                                KeliCarryfulTagExpr tag _  -> undefined
                                KeliTagUnionExpr tags      -> Right tags 
                                _                          -> undefined
                        in case mapM extractTags typeCheckedParams of
                            Right tags -> Right (KeliTagUnionExpr (intercalate [] tags))
                            Left err   -> Left err
                         ) 
                else
                    typeCheckFuncCall typeCheckedParams ids
        _ 
            ->
            case head params of
            KeliId firstParamToken@(_,firstParamId) 
                -> 
                case firstParamId of
                -- 1. Check if user wants to create a tag
                "_"
                    ->
                    case ids !! 0 of 
                    (_,"tag")
                        ->
                        if length params < 2 then
                            Left (KErrorIncorrectUsageOfTag)
                        else
                            case params !! 1 of
                                (KeliId tag)
                                    ->
                                    -- 1.1 Check if user wants to create carryless/carryful tag
                                    if length ids < 2
                                    then -- carryless tag
                                        Right (KeliCarrylessTagExpr tag)
                                    else if snd (ids !! 1) == "carry"
                                    then -- carryful tag
                                        undefined
                                    else do 
                                        typeCheckedParams <- typeCheckExprs symtab params
                                        typeCheckFuncCall params ids
                                
                                _
                                    ->
                                    Left (KErrorIncorrectUsageOfTag)
                    _ 
                        -> do
                        typeCheckedParams <- typeCheckExprs symtab params
                        typeCheckFuncCall params ids

                -- 2. Check if the user wants to create a record (type/value)
                "record"
                    ->  
                    if tail params == [] then 
                        Left (KErrorIncorrectUsageOfRecord firstParamToken)
                    else if isType symtab (params !! 1) then 
                        -- assume user want to declare a record type
                        let types = tail params in do
                            resolvedTypes <- resolveTypes symtab types
                            return 
                                (KeliTypeExpr
                                    (KeliTypeRecord (zip ids resolvedTypes)))

                    else -- assume user want to create an anonymous record
                        let values = tail params in do
                            typeCheckedExprs <- typeCheckExprs symtab values
                            return 
                                (KeliTypeCheckedExpr 
                                    (KeliRecord (zip ids typeCheckedExprs))
                                    (KeliTypeRecord (zip ids (map getType typeCheckedExprs))))
            _ 
                -> do
                typeCheckedParams <- typeCheckExprs symtab params
                -- 3. Check if user are calling record getter/setter function
                let subject = typeCheckedParams !! 0 in
                    case getType subject of
                        KeliTypeRecord kvs 
                            ->  
                            let funcId = intercalate "$" (map snd ids) in
                            case find (\(token@(_,key),_) -> key == funcId) kvs of
                                Just (token, expectedType) 
                                    -> 
                                    -- 3.1 Check if is getter or setter
                                    if tail typeCheckedParams == [] 
                                    then 
                                        -- 3.1.1 is getter
                                        Right (KeliTypeCheckedExpr (KeliRecordGetter subject token) expectedType) 
                                    else 
                                        -- 3.1.2 is setter
                                        -- 3.1.2.1 check is setter value has the correct type
                                        let [newValue] = tail typeCheckedParams in
                                        if getType newValue == expectedType 
                                        then 
                                            Right (KeliTypeCheckedExpr (KeliRecordSetter subject token newValue) expectedType) 
                                        else
                                            Left KErrorWrongTypeInSetter

                                Nothing 
                                    -> 
                                    -- If not, treat is as a normal function call
                                    typeCheckFuncCall typeCheckedParams ids
                        _ 
                            -> 
                                -- If not, treat is as a normal function call
                                typeCheckFuncCall typeCheckedParams ids
    where 
        getFuncId (KeliFuncCall params ids) 
            = intercalate "$" (map snd ids) ++ intercalate "$" (map (toString . getType) params)
        
        typeCheckFuncCall :: [KeliExpr] -> [StringToken] -> Either KeliError KeliExpr
        typeCheckFuncCall params ids = 
            let funcCall = KeliFuncCall params ids in
                case lookup (getFuncId funcCall) symtab of
                    Just (KeliSymFunc f) 
                        -> Right (KeliTypeCheckedExpr funcCall (funcDeclReturnType f))
                    _ 
                        -> Left (KErrorUsingUndefinedFunc ids)


resolveType :: KeliSymTab -> KeliExpr -> Either KeliError KeliType
resolveType symtab expr =
    case expr of
        (KeliId (_,id)) 
            -> case lookup id symtab of
                Just (KeliSymType t) -> Right t
                Nothing              -> Left (KErrorUsingUndefinedType)
        -- In the future need to handle function call, e.g. `a.list`
        _ 
            -> Left (KErrorUsingUndefinedType)

typeCheckExprs :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliExpr]
typeCheckExprs symtab exprs = 
    foldM 
    (\acc next -> do
        typeChecked <- typeCheckExpr symtab next 
        return (acc ++ [typeChecked]))
    [] 
    exprs

resolveTypes :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliType]
resolveTypes symtab exprs =
    foldM 
    (\acc next -> do
        resolvedType <- resolveType symtab next 
        return (acc ++ [resolvedType]))
    [] 
    exprs

    
isType :: KeliSymTab -> KeliExpr -> Bool
isType symtab expr = 
    case expr of
        (KeliId (_,id)) 
            -> case lookup id symtab of
                Just (KeliSymType _) -> True
                Nothing              -> False
        -- In the future need to handle function call, e.g. `a.list`
        _ 
            -> False
