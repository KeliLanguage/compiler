{-# LANGUAGE MultiParamTypeClasses #-}
module Analyzer where


import Ast
import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered ((|>), assocs, member, lookup) 
import Data.Set (Set,fromList,difference,toList)
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)
import StaticError
import SymbolTable
import Data.Maybe (catMaybes, fromJust)

analyze :: KeliSymTab -> Either KeliError [KeliSym]
analyze symtab = do
    let symbols         = map snd (assocs symtab)
    let constSymbols    = filter (\x -> case x of KeliSymConst _ -> True; _->False) symbols 
    symtab'            <- analyzeSymbols constSymbols symtab
    let funcSymbols     = filter (\x -> case x of KeliSymFunc _ -> True; _->False) symbols 
    finalSymtab        <- analyzeSymbols funcSymbols symtab'
    let analyzedSymbols = map snd (assocs finalSymtab)
    -- sorting is necessary, so that the transpilation order will be correct
    -- Smaller number means will be transpiled first
    let sortedSymbols = sortOn (
            \x -> case x of 
                KeliSymSingleton _ -> 0
                KeliSymTag _       -> 1
                KeliSymFunc _      -> 2
                KeliSymConst _     -> 3
                KeliSymType _      -> 4
            ) analyzedSymbols
    return sortedSymbols 


analyzeSymbols :: [KeliSym] -> KeliSymTab -> Either KeliError KeliSymTab
analyzeSymbols symbols symtab0 = 
    foldM
    ((\symtab1 nextSym1 -> do
        (analyzedSymbol, extraSymbols) <- analyzeSymbol nextSym1 symtab1

        -- insert the const symbol into symtab
        let id@(_,key) = getIdentifier nextSym1
        let symtab2 = symtab1 |> (key, analyzedSymbol)

        -- insert extra symbols into symtab (i.e., tags)
        let symtab3 = 
                foldM
                ((\symtab4 nextSym2 -> 
                        let token@(_,key') = getIdentifier nextSym2 in
                        let annotatedSymbol = case nextSym2 of
                                KeliSymTag (KeliTagCarryless tag _) -> KeliSymTag (KeliTagCarryless tag (KeliTypeAlias id))
                                _ -> undefined 
                        in
                        if member key' symtab4 then
                            Left (KErrorDuplicatedId token)
                        else
                            Right (symtab4 |> (key', annotatedSymbol))
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
                    return (KeliSymType (KeliTypeTagUnion [tag]), [ KeliSymTag (KeliTagCarryless tag KeliTypeUndefined) ])
                
                KeliCarryfulTagExpr _ _
                    -> undefined
                
                KeliTagUnionExpr tags
                    -> 
                    let extractTags x = case x of KeliTagCarryless t _ -> t; KeliTagCarryful t _ _ -> t; in
                    return (KeliSymType (KeliTypeTagUnion (map extractTags tags)), map KeliSymTag tags)
                _              
                    -> return (KeliSymConst (KeliConst id checkedExpr expectedType),[])

    KeliSymFunc (func@KeliFunc {
        funcDeclParams=params,
        funcDeclReturnType=returnType,
        funcDeclBody=body
    }) -> do 
            -- 0. Verify annotated types of each func param
            verifiedParams <- mapM
                (\x -> do 
                    verifiedType <- verifyType symtab (extractExprOfUnverifiedType (funcDeclParamType x))
                    return x {funcDeclParamType = verifiedType}
                ) params

            -- 1. verify return type
            verifiedReturnType <- verifyType symtab (extractExprOfUnverifiedType returnType)

            -- 2. populate symbol table with function parameters
            symtab' <- 
                foldM 
                ((\acc (KeliFuncDeclParam id expectedType) -> 
                    -- Resolve type alias. Why not resolve earlier? (Because transpiler need type alias to work)
                    let expectedType' = resolveAlias expectedType symtab in

                    let id' = snd id in
                    if member id' acc 
                        then Left (KErrorDuplicatedId id)
                        else Right(
                            let keyValue = (id', (KeliSymConst (KeliConst id (KeliTypeCheckedExpr (KeliId id) expectedType') Nothing)))
                            in acc |> keyValue
                                ))::KeliSymTab -> KeliFuncDeclParam -> Either KeliError KeliSymTab)
                symtab
                verifiedParams
            
            -- 3. type check the function body
            typeCheckedBody <- typeCheckExpr symtab' body
        
            -- 4. ensure body type adheres to return type
            if typeEquals (getType typeCheckedBody) (verifiedReturnType) symtab then
                Right (KeliSymFunc (func {
                        funcDeclBody = typeCheckedBody,
                        funcDeclParams = verifiedParams,
                        funcDeclReturnType = verifiedReturnType
                    }),[])
            else
                Left (KErrorUnmatchingFuncReturnType (getType typeCheckedBody) verifiedReturnType)
    
    _ -> undefined

typeCheckExpr :: KeliSymTab -> KeliExpr  -> Either KeliError KeliExpr
typeCheckExpr symtab e = 
    case typeCheckExpr' symtab e of 
        expr@(Right KeliTypeCheckedExpr{}) -> expr
        expr@(Right KeliCarrylessTagExpr{}) -> expr
        expr@(Right KeliTagUnionExpr{}) -> expr
        expr@(Right _) -> error ("return type of typeCheckExpr' should be KeliTypeCheckedExpr but received " ++ show expr)
        Left err -> Left err


typeCheckExpr' :: KeliSymTab -> KeliExpr  -> Either KeliError KeliExpr
typeCheckExpr' symtab e = case e of 
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
        
            Just (KeliSymSingleton token')
                -> Right (KeliTypeCheckedExpr expr (KeliTypeSingleton token'))

            Just (KeliSymTag (KeliTagCarryless _ type'))
                -> Right (KeliTypeCheckedExpr expr type')

            Just (KeliSymTag (KeliTagCarryful _ _ _))
                -> undefined
            _ 
                -> Left (KErrorUsingUndefinedId token))

    -- NOTES:
    -- * Why isn't func call params type checked at the very beginning?
    --      Because we need to cater for the magic `_.tag XXX` and `record.XXX YYY` function
    --      That's why you will see a lot of duplicated code for typeCheckExprs
    KeliFuncCall params funcIds
        -> 
        case head funcIds of 
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
                                KeliCarryfulTagExpr _ _    -> undefined
                                KeliTagUnionExpr tags      -> Right tags 
                                _                          -> undefined
                        in case mapM extractTags typeCheckedParams of
                            Right tags -> Right (KeliTagUnionExpr (intercalate [] tags))
                            Left err   -> Left err
                        ) 
                else
                    typeCheckFuncCall typeCheckedParams funcIds
        _ 
            ->
            case head params of
            KeliId firstParamToken@(_,firstParamId) 
                -> 
                case firstParamId of
                -- 1. Check if user wants to create a tag
                "_"
                    ->
                    case funcIds !! 0 of 
                    (_,"tag")
                        ->
                        if length params < 2 then
                            Left (KErrorIncorrectUsageOfTag)
                        else
                            case params !! 1 of
                                (KeliId tag)
                                    ->
                                    -- 1.1 Check if user wants to create carryless/carryful tag
                                    if length funcIds < 2
                                    then -- carryless tag
                                        Right (KeliCarrylessTagExpr tag)
                                    else if snd (funcIds !! 1) == "carry"
                                    then -- carryful tag
                                        undefined
                                    else do 
                                        typeCheckedParams <- typeCheckExprs symtab params
                                        typeCheckFuncCall typeCheckedParams funcIds
                                
                                _
                                    ->
                                    Left (KErrorIncorrectUsageOfTag)
                    _ 
                        -> do
                        typeCheckedParams <- typeCheckExprs symtab params
                        typeCheckFuncCall typeCheckedParams funcIds

                -- 2. Check if the user wants to create a record (type/value)
                "record"
                    ->  
                    if tail params == [] then 
                        Left (KErrorIncorrectUsageOfRecord firstParamToken)
                    else if isType symtab (params !! 1) then 
                        -- assume user want to declare a record type
                        let types = tail params in do
                            resolvedTypes <- verifyTypes symtab types
                            return (KeliTypeExpr (KeliTypeRecord (zip funcIds resolvedTypes)))

                    else -- assume user want to create an anonymous record
                        let values = tail params in do
                            typeCheckedExprs <- typeCheckExprs symtab values
                            return 
                                (KeliTypeCheckedExpr 
                                    (KeliRecord (zip funcIds typeCheckedExprs))
                                    (KeliTypeRecord (zip funcIds (map getType typeCheckedExprs))))
                
                _ 
                    -> do 
                    typeCheckedParams <- typeCheckExprs symtab params
                    typeCheckFuncCall typeCheckedParams funcIds

            _ 
                -> do
                typeCheckedParams <- typeCheckExprs symtab params
                -- 3. Check if user are calling record getter/setter function
                let subject = typeCheckedParams !! 0 in
                    case getType subject of
                        KeliTypeRecord kvs 
                            ->  
                            let funcId = intercalate "$" (map snd funcIds) in
                            case find (\((_,key),_) -> key == funcId) kvs of
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
                                    typeCheckFuncCall typeCheckedParams funcIds

                        _ 
                            -> 
                            typeCheckFuncCall typeCheckedParams funcIds

    _ 
        -> 
        undefined

    where 
        getFuncId (KeliFuncCall params funcIds) = intercalate "$" (map snd funcIds) ++ intercalate "$" (map (toString . getType) params)
        getFuncId _ = undefined
        
        -- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
        typeCheckFuncCall :: [KeliExpr] -> [StringToken] -> Either KeliError KeliExpr
        typeCheckFuncCall params funcIds = 
            -- check if user are calling tag matchers
            case getType (head params) of
                KeliTypeTagUnion tags
                    -> 
                    let funcIds' = map (init . snd) funcIds in
                    let tags'    = map snd tags in
                    let intersection = intersect funcIds' tags' in
                    let branches = tail params in
                    let firstBranch = head branches in
                    let subject = head params in

                    if length intersection == 0 then
                        -- treat as regular function call
                        typeCheckFuncCall' params funcIds

                    -- check for duplicated tags
                    else if length (nub funcIds') /= length funcIds' then
                        Left (KErrorDuplicatedTags funcIds)
                    
                    
                    -- check if all branch have the same type
                    else if any (\x -> not (typeEquals (getType x) (getType firstBranch) symtab)) branches then
                        Left (KErrorNotAllBranchHaveTheSameType (zip funcIds branches))

                    else if length intersection == length tags then (
                        -- check for excessive tags
                        if length funcIds > length tags then
                            let excessiveCases = 
                                    catMaybes (map 
                                        (\x -> find (\y -> snd y == x) funcIds) 
                                        (toList (difference (fromList funcIds') (fromList tags'))))
                            in Left (KErrorExcessiveTags excessiveCases)
                        else 
                            -- complete tag matches
                            Right (KeliTypeCheckedExpr (KeliTagMatcher subject (zip funcIds branches) Nothing) (getType (head branches)))
                    )
                    else if length intersection < length tags then (
                        if "else" `elem` funcIds' then
                            let tagBranches = zip funcIds branches in
                            let elseBranch = fromJust (find (\((_,id),_) -> id == "else?") tagBranches) in
                            let otherBranches = filter (\((_,id),_) -> id /= "else?") tagBranches in

                            Right (KeliTypeCheckedExpr (KeliTagMatcher subject otherBranches (Just (snd elseBranch))) (getType (head branches)))
                        else -- missing tags
                            Left (KErrorMissingTags (pTraceShowId (tags' \\ funcIds')))
                    )
                    else
                        -- treat as regular function call
                        typeCheckFuncCall' params funcIds
                _ 
                    ->
                    -- treat as regular function call
                    typeCheckFuncCall' params funcIds
            
        typeCheckFuncCall' :: [KeliExpr] -> [StringToken] -> Either KeliError KeliExpr
        typeCheckFuncCall' params funcIds = 
            let funcCall = KeliFuncCall params funcIds in
                case lookup (getFuncId funcCall) symtab of
                    Just (KeliSymFunc f) 
                        -> Right (KeliTypeCheckedExpr funcCall (funcDeclReturnType f))
                    _ 
                        -> Left (KErrorUsingUndefinedFunc funcIds)


verifyType :: KeliSymTab -> KeliExpr -> Either KeliError KeliType
verifyType symtab expr =
    case expr of
        (KeliId token@(_,id)) 
            -> case lookup id symtab of
                Just (KeliSymType _) -> Right (KeliTypeAlias token)
                Nothing              -> Left (KErrorUsingUndefinedType expr)
                _                    -> undefined
        -- In the future need to handle function call, e.g. `a.list`
        _ 
            -> Left (KErrorUsingUndefinedType expr)

typeCheckExprs :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliExpr]
typeCheckExprs symtab exprs = 
    foldM 
    (\acc next -> do
        typeChecked <- typeCheckExpr symtab next 
        return (acc ++ [typeChecked]))
    [] 
    exprs

verifyTypes :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliType]
verifyTypes symtab exprs =
    foldM 
    (\acc next -> do
        resolvedType <- verifyType symtab next 
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
                _                    -> undefined
        -- In the future need to handle function call, e.g. `a.list`
        _ 
            -> False

extractExprOfUnverifiedType :: KeliType -> KeliExpr
extractExprOfUnverifiedType x = 
    case x of 
        KeliTypeUnverified t -> t
        _ -> undefined

typeEquals :: KeliType -> KeliType -> KeliSymTab -> Bool
typeEquals x y symtab = 
    let x' = resolveAlias x symtab in
    let y' = resolveAlias y symtab in
    x' == y'

resolveAlias :: KeliType -> KeliSymTab -> KeliType
resolveAlias a symtab = case a of 
    KeliTypeAlias (_,id) -> case fromJust (lookup id symtab) of KeliSymType t -> t; _ -> undefined;
    other@_ -> other