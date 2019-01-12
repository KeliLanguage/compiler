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
import Util
import Data.Maybe (catMaybes, fromJust)

analyze :: KeliSymTab -> Either KeliError [KeliSym]
analyze symtab = do
    let symbols         = map snd (assocs symtab)
    finalSymtab        <- analyzeSymbols symbols 
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


analyzeSymbols :: [KeliSym] ->  Either KeliError KeliSymTab
analyzeSymbols symbols = 
    foldM
    ((\symtab1 nextSym1 -> do
        (analyzedSymbol, extraSymbols) <- analyzeSymbol nextSym1 symtab1

        -- insert the const symbol into symtab
        let id@(_,key) = 
                case analyzedSymbol of 
                    KeliSymFunc f -> getIdentifier f
                    _ -> getIdentifier nextSym1

        let symtab2 = symtab1 |> (key, analyzedSymbol)

        -- insert extra symbols into symtab (i.e., tags)
        let symtab3 = 
                foldM
                ((\symtab4 nextSym2 -> 
                        let token@(_,key') = getIdentifier nextSym2 in
                        let annotatedSymbol = case nextSym2 of
                                KeliSymTag (KeliTagCarryless tag _)      -> KeliSymTag (KeliTagCarryless tag (KeliTypeAlias id))
                                KeliSymTag (KeliTagCarryful tag carry _) -> KeliSymTag (KeliTagCarryful tag carry (KeliTypeAlias id))
                                _ -> undefined 
                        in
                        if member key' symtab4 then
                            Left (KErrorDuplicatedId token)
                        else
                            Right (symtab4 |> (key', annotatedSymbol))
                    ))
                (symtab2)
                extraSymbols

        symtab3
    )::KeliSymTab -> KeliSym -> Either KeliError KeliSymTab)
    emptyKeliSymTab
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

                KeliCarrylessTagDeclExpr tag
                    -> 
                    return (KeliSymType (KeliTypeTagUnion [tag]), [ KeliSymTag (KeliTagCarryless tag KeliTypeUndefined) ])
                
                KeliCarryfulTagDeclExpr _ _
                    -> undefined
                
                KeliTagUnionDeclExpr tags
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
                    let id' = snd id in
                    if member id' acc 
                        then Left (KErrorDuplicatedId id)
                        else Right(
                            let keyValue = (id', (KeliSymConst (KeliConst id (KeliTypeCheckedExpr (KeliId id) expectedType) Nothing)))
                            in acc |> keyValue
                                ))::KeliSymTab -> KeliFuncDeclParam -> Either KeliError KeliSymTab)
                symtab
                verifiedParams
            
            -- 3. type check the function body
            typeCheckedBody <- typeCheckExpr symtab' body
        
            -- 4. ensure body type adheres to return type
            -- resolve type alias
            let verifiedReturnType' = getType symtab verifiedReturnType

            let bodyType = getType symtab typeCheckedBody
            let result = Right (KeliSymFunc (func {
                                    funcDeclBody = typeCheckedBody,
                                    funcDeclParams = verifiedParams,
                                    funcDeclReturnType = verifiedReturnType
                                }),[])

            if bodyType == verifiedReturnType' then
                result
            else 
                case bodyType of
                    KeliTypeSingleton (_,"undefined") -> result
                    _ -> Left (KErrorUnmatchingFuncReturnType (getType symtab typeCheckedBody) verifiedReturnType')
    
    _ -> Right (symbol, [])

typeCheckExpr :: KeliSymTab -> KeliExpr  -> Either KeliError KeliExpr
typeCheckExpr symtab e = 
    case typeCheckExpr' symtab e of 
        expr@(Right KeliTypeCheckedExpr{})      -> expr
        expr@(Right KeliCarrylessTagDeclExpr{}) -> expr
        expr@(Right KeliCarryfulTagDeclExpr{})  -> expr
        expr@(Right KeliTagUnionDeclExpr{})     -> expr
        expr@(Right KeliTypeExpr{})             -> expr
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
        case lookup id symtab of 
            Just (KeliSymConst (KeliConst _ (KeliTypeCheckedExpr _ exprType) _))
                -> Right (KeliTypeCheckedExpr expr exprType)
        
            Just (KeliSymSingleton token')
                -> Right (KeliTypeCheckedExpr expr (KeliTypeSingleton token'))

            Just (KeliSymTag (KeliTagCarryless tag belongingType))
                -> Right (KeliTypeCheckedExpr (KeliTagConstructor tag Nothing) belongingType)

            Just (KeliSymTag (KeliTagCarryful tag carryType belongingType))
                -- How to check if user forgot to put .carry ?
                --  Don't need to explicitly check it, the type system will handle it
                -> Right (KeliTypeCheckedExpr expr (KeliTypeCarryfulTagConstructor tag carryType belongingType))
            
            Just (KeliSymType (KeliTypeRecord propTypePairs))
                -> Right (KeliTypeCheckedExpr (KeliRecordConstructor propTypePairs) (KeliTypeRecordConstructor propTypePairs))

            Nothing 
                -> Left (KErrorUsingUndefinedId token)
            
            _
                -> undefined


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
                let isTagOrUnion x = case x of KeliCarrylessTagDeclExpr _  -> True; KeliCarryfulTagDeclExpr _ _ -> True; KeliTagUnionDeclExpr _ -> True; _ -> False;
                if isTagOrUnion (head typeCheckedParams) then (
                    if length typeCheckedParams /= 2 then
                        Left KErrorIncorrectUsageOfTaggedUnion
                    else if any (not . isTagOrUnion) typeCheckedParams then
                        Left KErrorIncorrectUsageOfTaggedUnion
                    else
                        let extractTags x = case x of
                                KeliCarrylessTagDeclExpr tag      -> Right [KeliTagCarryless tag KeliTypeUndefined]
                                KeliCarryfulTagDeclExpr tag carry -> Right [KeliTagCarryful tag carry KeliTypeUndefined]
                                KeliTagUnionDeclExpr tags         -> Right tags 
                                _                                 -> undefined
                        in case mapM extractTags typeCheckedParams of
                            Right tags -> Right (KeliTagUnionDeclExpr (intercalate [] tags))
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
                                        Right (KeliCarrylessTagDeclExpr tag)
                                    else if snd (funcIds !! 1) == "carry"
                                    then do-- carryful tag
                                        carryType <- verifyType symtab (params !! 2)
                                        Right (KeliCarryfulTagDeclExpr tag carryType)
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

                    else -- assume user want to create an record value
                        let values = tail params in do
                            typeCheckedExprs <- typeCheckExprs symtab values
                            return 
                                (KeliTypeCheckedExpr 
                                    (KeliRecord (zip funcIds typeCheckedExprs))
                                    (KeliTypeRecord (zip funcIds (map (getType symtab) typeCheckedExprs))))
                
                _ 
                    -> do 
                    typeCheckedParams <- typeCheckExprs symtab params
                    typeCheckFuncCall typeCheckedParams funcIds

            _ 
                -> do
                typeCheckedParams <- typeCheckExprs symtab params
                -- 3. Check if user are calling record getter/setter function
                let subject = typeCheckedParams !! 0 in
                    case getType symtab subject of
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
                                        if getType symtab newValue == expectedType 
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
        -- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
        typeCheckFuncCall :: [KeliExpr] -> [StringToken] -> Either KeliError KeliExpr
        typeCheckFuncCall params funcIds = 
            let typeOfFirstParam = getType symtab (head params) in
            case  typeOfFirstParam of
                -- (A) check if user is invoking record constructor
                KeliTypeRecordConstructor propTypePairs -> 
                    let expectedProps = map fst propTypePairs in
                    let actualProps = funcIds in
                    let values     = tail params in
                    case match actualProps expectedProps of
                        GotDuplicates ->
                            Left KErrorDuplicatedProperties

                        ZeroIntersection ->
                            -- treat as regular function call
                            typeCheckFuncCall' params funcIds
                        
                        GotExcessive excessiveProps ->
                            Left (KErrorExcessiveProperties excessiveProps)
                        
                        Missing missingProps ->
                            Left (KErrorMissingProperties missingProps)
                        
                        PerfectMatch -> 
                            let expectedPropTypePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) propTypePairs in
                            let actualPropValuePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) (zip actualProps values) in
                            case find (\(expected, actual) -> 
                                    let expectedType = getType symtab (snd expected) in
                                    let actualType = getType symtab (snd actual) in
                                    expectedType /= actualType
                                ) (zip expectedPropTypePairs actualPropValuePairs) of
                                Just (expected, actual) -> 
                                    Left (KErrorPropretyTypeMismatch (fst expected) (snd expected) (snd actual))
                                Nothing -> 
                                    Right (KeliTypeCheckedExpr (KeliRecord actualPropValuePairs) (KeliTypeRecord propTypePairs))

                                
                        

                -- (B) check if user is invoking carryful tag constructor
                KeliTypeCarryfulTagConstructor tag carryType belongingType
                    -> 
                    if length funcIds == 1 && (snd (funcIds !! 0)) == "carry" then
                        let carryExpr     = params !! 1 in
                        let carryExprType = getType symtab carryExpr in
                        let carryType'    = getType symtab carryType in
                        if carryExprType == carryType' then
                            Right (KeliTypeCheckedExpr (KeliTagConstructor tag (Just carryExpr)) belongingType)
                        else
                            Left (KErrorIncorrectCarryType carryType' carryExpr)
                    else
                        -- treat as regular function call
                        typeCheckFuncCall' params funcIds
                    
                
                -- (C) check if user is calling tag matchers
                KeliTypeTagUnion tags
                    -> 
                    let branches = tail params in
                    let firstBranch = head branches in
                    let subject = head params in
                    let tagsWithQuestionMark = map (\(pos,id) -> (pos,id ++ "?")) tags in
                    case match funcIds tagsWithQuestionMark of
                        GotDuplicates -> 
                            Left (KErrorDuplicatedTags funcIds)

                        ZeroIntersection -> 
                            -- treat as regular function call
                            typeCheckFuncCall' params funcIds
                        
                        GotExcessive excessiveCases ->
                            Left (KErrorExcessiveTags excessiveCases)
                        
                        Missing cases ->
                            if "else" `elem` (map snd funcIds) then
                                let tagBranches = zip funcIds branches in
                                let elseBranch = fromJust (find (\((_,id),_) -> id == "else?") tagBranches) in
                                let otherBranches = filter (\((_,id),_) -> id /= "else?") tagBranches in

                                Right (KeliTypeCheckedExpr (KeliTagMatcher subject otherBranches (Just (snd elseBranch))) (getType symtab (head branches)))
                            else -- missing tags
                                Left (KErrorMissingTags cases)

                        PerfectMatch ->
                            -- check if all branch have the same type
                            if any (\x -> getType symtab x /= getType symtab firstBranch) branches then
                                Left (KErrorNotAllBranchHaveTheSameType (zip funcIds branches))
                            else
                                -- complete tag matches
                                Right (KeliTypeCheckedExpr (KeliTagMatcher subject (zip funcIds branches) Nothing) (getType symtab (head branches)))
                _ 
                    ->
                    -- treat as regular function call
                    typeCheckFuncCall' params funcIds
            
        typeCheckFuncCall' :: [KeliExpr] -> [StringToken] -> Either KeliError KeliExpr
        typeCheckFuncCall' params funcIds = 
            let funcCall = KeliFuncCall params funcIds in
            let funcId = getFuncIdFromFuncCall funcCall in
            case lookup funcId symtab of
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

getFuncIdFromFuncCall :: KeliExpr -> String
getFuncIdFromFuncCall (KeliFuncCall params funcIds) = 
    intercalate "$" (map (toValidJavaScriptId . snd) funcIds) ++ intercalate "$" (map (toString . getTypeWithoutResolvingAlias) params)
getFuncIdFromFuncCall _ = undefined


getTypeWithoutResolvingAlias :: KeliExpr -> KeliType
getTypeWithoutResolvingAlias e = case e of
    KeliTypeCheckedExpr _ t -> t
    _ -> undefined
    