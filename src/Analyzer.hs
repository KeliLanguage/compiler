{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Analyzer where


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


data Assumption = AnalyzingExpr | AnalyzingType -- this is needed to determine whether singleton is used as type or expr

-- preprocessDecls will apply magic constant/function (such as _.tag, record etc.) to the AST and transform it
-- NOTE: preprocessDecls will not do any type checking if not needed
preprocessDecls :: KeliSymTab -> [KeliDecl] -> Either KeliError [KeliDecl]
preprocessDecls symtab decls = mapM (preprocessDecl symtab) decls

preprocessDecl :: KeliSymTab -> KeliDecl -> Either KeliError KeliDecl
preprocessDecl symtab decl = 
    case decl of
        KeliConstDecl c@(KeliConst id value _) -> 
            case value of
                KeliId s@(_,id') -> 
                    if snd id == id' then 
                        Right (KeliTypeAliasDecl [s] (KeliTypeSingleton s))
                    else if id' == "_primitive_type" then
                        case snd id of
                            "int"   -> Right (KeliTypeAliasDecl [id] KeliTypeInt)
                            "str"   -> Right (KeliTypeAliasDecl [id] KeliTypeString)
                            "float" -> Right (KeliTypeAliasDecl [id] KeliTypeFloat)
                            "type"  -> Right (KeliTypeAliasDecl [id] KeliTypeType)
                            other   -> error("Unkown primitive type: " ++ other)
                    else if id' == "_primitive_constraint" then
                        case snd id of
                            "any" -> Right (KeliTypeAliasDecl [id] (KeliTypeConstraint KeliConstraintAny))
                            other -> error ("Unknown primitive constraint type: " ++ other)
                    else 
                        continuePreprocessConstDecl
                
                _ -> 
                    continuePreprocessConstDecl

                where 
                    continuePreprocessConstDecl = 
                        case preprocessExpr symtab value of 
                            Right (First expr) -> 
                                Right (KeliConstDecl (c {constDeclValue = expr}))

                            Right (Second type') ->
                                Right (KeliTypeAliasDecl [id] type')
                            
                            Right (Third tag) ->
                                Right (KeliTypeAliasDecl [id] (KeliTypeTagUnion [tag]))
                            
                            Left err -> Left err

        KeliFuncDecl f@(KeliFunc genParams params ids returnType body) ->
            case preprocessExpr symtab body of
                Right (First expr) ->
                    Right (KeliFuncDecl (f {funcDeclBody = expr}))
                
                Right (Second type') ->
                    Right (KeliTypeAliasDecl ids type')
                
                Right (Third tag) ->
                    undefined

                Left err -> Left err

        KeliIdlessDecl expr -> do
            result <- preprocessExpr symtab expr
            case result of
                First preprocessedExpr ->
                    Right (KeliIdlessDecl preprocessedExpr)

                Second type' ->
                    Left (KErrorCannotDeclareTypeAsAnonymousConstant type')
                
                Third tag ->
                    Left (KErrorCannotDeclareTagAsAnonymousConstant tag)


data OneOf3 a b c = First a | Second b | Third c

preprocessExpr :: KeliSymTab -> KeliExpr -> Either KeliError (OneOf3 KeliExpr KeliType KeliTag)
preprocessExpr symtab expr =
    case expr of
    KeliId id ->
        case lookup (snd id) symtab of
            Just (KeliSymConst (KeliConst _ value _)) -> 
                Right (First value)
            
            Just (KeliSymType t) ->
                Right (Second t)
            
            Just (KeliSymTag tag) ->
                case tag of
                    KeliTagCarryless name belongingType ->
                        Right (First (KeliTypeCheckedExpr (KeliTagConstructor name Nothing) belongingType))
                    
                    KeliTagCarryful name carryType belongingType ->
                        Right (Second (KeliTypeCarryfulTagConstructor name carryType belongingType))
            
            other ->
                error (show other ++ show id)

    KeliFuncCall params' funcIds ref -> do
        params <- mapM (preprocessExpr symtab) params'
        extractedExprs <- mapM extractExpr params
        let normalFuncCall = Right (First (KeliFuncCall extractedExprs funcIds ref))

        case head funcIds of 
            -- 0.B Check if user wants to create a tagged union
            (_,"or") -> do
                let isTagOrUnion x = 
                        case x of 
                            Second (KeliTypeTagUnion{})-> True; 
                            Third _ -> True; 
                            _ -> False;
                if isTagOrUnion (head params) then (
                    if length params /= 2 then
                        Left (KErrorIncorrectUsageOfTaggedUnion expr)
                    else if any (not . isTagOrUnion) params then
                        Left (KErrorIncorrectUsageOfTaggedUnion expr)
                    else
                        Right (Second (KeliTypeTagUnion (concat (map extractTag params)))))
                else 
                    normalFuncCall

            _ ->
                case head params of
                First (KeliId firstParamToken@(_,firstParamId)) -> 
                    case firstParamId of
                    -- 1. Check if user wants to create a tag
                    "_" ->
                        case funcIds !! 0 of 
                        (_,"tag") ->
                            if length params < 2 then
                                Left (KErrorIncorrectUsageOfTag expr)
                            else
                                case params !! 1 of
                                    (First (KeliId tag)) ->
                                        -- 1.1 Check if user wants to create carryless/carryful tag
                                        if length funcIds < 2 then -- carryless tag
                                            Right (Third (KeliTagCarryless tag KeliTypeUndefined))

                                        else if snd (funcIds !! 1) == "carry" then do -- carryful tag
                                            if length params < 3 then
                                                Left (KErrorIncorrectUsageOfTag expr)
                                            else
                                                case params !! 2 of
                                                    Second carryType ->
                                                        Right (Third (KeliTagCarryful tag carryType KeliTypeUndefined))
                                                    
                                                    _ ->
                                                        Left (KErrorIncorrectUsageOfTag expr)
                                        else
                                            Left (KErrorIncorrectUsageOfTag expr)
                                    
                                    _ ->
                                        Left (KErrorIncorrectUsageOfTag expr)

                        _ ->
                            normalFuncCall

                    -- 2. Check if the user wants to create a record (type/value)
                    "record" ->  
                        if length (tail params) == 0 then 
                            Left (KErrorIncorrectUsageOfRecord firstParamToken)

                        else (
                            let values = tail params in 
                            case values !! 0 of
                                -- assume user want to create a record value
                                First _ -> do
                                    exprs <- mapM extractExpr values
                                    Right (First (KeliRecord (zip funcIds exprs)))
                                
                                -- assume user want to declare a record type
                                Second _ -> do
                                    types <- mapM extractType values
                                    let keys = funcIds
                                    Right (Second (KeliTypeRecord (zip keys types)))
                        )
                    _ -> 
                        normalFuncCall

                _ -> 
                    normalFuncCall

    _ -> Right (First expr)


extractTag :: OneOf3 KeliExpr KeliType KeliTag -> [KeliTag]
extractTag x =
    case x of
        Second (KeliTypeTagUnion tags) -> tags
        Third tag -> [tag]
        _ -> error "impossible" 

extractType :: OneOf3 KeliExpr KeliType KeliTag -> Either KeliError KeliType
extractType x = 
    case x of
        First expr   -> Left (KErrorExprIsNotAType expr)
        Second type' -> Right type' 
        Third tag    -> Left (KErrorTagIsNotAType tag)



extractExpr :: OneOf3 KeliExpr KeliType KeliTag -> Either KeliError KeliExpr
extractExpr x = 
    case x of 
        First expr -> Right expr
        Second type' -> Left (KErrorTypeIsNotAnExpr type')
        Third tag -> Left (KErrorTagIsNotAnExpr tag)

analyze :: [KeliDecl] -> Either KeliError [KeliSymbol]
analyze decls = do
    finalSymtab        <- analyzeDecls decls
    let analyzedSymbols = map snd (assocs finalSymtab)

    -- sorting is necessary, so that the transpilation order will be correct
    -- Smaller number means will be transpiled first
    let sortedSymbols = sortOn (
            \x -> case x of 
                KeliSymSingleton _  -> 0
                KeliSymTag _        -> 1
                KeliSymFunc _       -> 2
                KeliSymConst _      -> 3
                KeliSymType _       -> 4
                KeliSymInlineExprs _-> 5
            ) analyzedSymbols
    return sortedSymbols 


analyzeDecls :: [KeliDecl] ->  Either KeliError KeliSymTab
analyzeDecls decls = 
    foldM
    ((\symtab1 nextDecl1 -> do
        preprocessedDecl <- preprocessDecl symtab1 nextDecl1
        analyzedSymbols <- analyzeDecl preprocessedDecl symtab1

        -- insert analyzedSymbols into symtab
        (foldM 
            (\symtab2 analyzedSymbol -> 
                case analyzedSymbol of 
                    KeliSymFunc [f] -> 
                        let funcid = (intercalate "$" (map snd (funcDeclIds f))) in
                        let funcsWithSameName = lookup funcid symtab2 in
                        let funcParamTypes = (\func -> map snd (funcDeclParams func)) in
                        case funcsWithSameName of
                            Just (KeliSymFunc fs) ->
                                if any (\func -> all (\(t1,t2) -> t1 `typeEquals` t2) (zip (funcParamTypes f) (funcParamTypes func))) fs then
                                    Left (KErrorDuplicatedFunc f)
                                else
                                    Right (symtab2 |> (funcid, KeliSymFunc (f:fs)))
                            
                            Just _ ->
                                Left (KErrorDuplicatedId (funcDeclIds f))

                            Nothing ->
                                Right (symtab2 |> (funcid, analyzedSymbol))

                    KeliSymInlineExprs exprs ->
                        case lookup "@inline_exprs" symtab2 of
                            Just (KeliSymInlineExprs exprs') ->
                                Right (symtab2 |> ("@inline_exprs", KeliSymInlineExprs (exprs' ++ exprs)))

                            Just _ ->
                                error "shouldn't reach here"
                            
                            Nothing ->
                                Right (symtab2 |> ("@inline_exprs", KeliSymInlineExprs exprs))

                    KeliSymType (KeliTypeSingleton _ ) -> -- do nothing
                        Right symtab2

                    _ -> 
                        let id@(_,key) = getIdentifier analyzedSymbol in
                        if member key symtab2 then
                            Left (KErrorDuplicatedId [id])
                        else 
                            Right (symtab2 |> (key, analyzedSymbol)))
            symtab1
            analyzedSymbols)
    )::KeliSymTab -> KeliDecl -> Either KeliError KeliSymTab)
    emptyKeliSymTab
    decls

analyzeDecl :: KeliDecl -> KeliSymTab -> Either KeliError [KeliSymbol]
analyzeDecl decl symtab = case decl of
    KeliConstDecl KeliConst {
        constDeclId=id,
        constDeclValue=expr,
        constDeclType=expectedType
    } -> 
        case expr of 
            _ ->
                continueAnalyzeConstDecl

        where 
            continueAnalyzeConstDecl = do
                typeCheckedExpr <- typeCheckExpr symtab expr
                return [KeliSymConst (KeliConst id typeCheckedExpr expectedType)]


    KeliFuncDecl(func@KeliFunc {
        funcDeclGenericParams = genericParams,
        funcDeclIds           = funcIds,
        funcDeclParams        = funcParams,
        funcDeclReturnType    = returnType,
        funcDeclBody          = funcBody
    }) -> do 
            let verifyParamType = 
                    (\tempSymtab params verify -> 
                        mapM
                        (\(id, paramType) -> do 
                            verifiedType <- verify tempSymtab paramType
                            return (id, verifiedType)
                        ) params
                    )

            let populateSymbolTable = (\_symtab params constructor -> 
                    foldM ((\acc (id, expectedType) -> 
                    let id' = snd id in
                    if member id' acc then 
                        Left (KErrorDuplicatedId [id])
                    else 
                        Right(
                            let keyValue = (id', constructor id expectedType)
                            in acc |> keyValue
                                ))::KeliSymTab -> KeliFuncDeclParam -> Either KeliError KeliSymTab)
                    _symtab
                    params)

            -- 0.1 Verify annotated constraint of each generic param
            verifiedGenericParams <- verifyParamType symtab genericParams verifyTypeConstraint

            -- 0.2 populate symbol table with generic type parameters
            symtab' <- 
                populateSymbolTable 
                symtab 
                verifiedGenericParams
                (\id paramType ->
                    case paramType of 
                        KeliTypeConstraint constraint -> 
                            KeliSymType (KeliTypeParam id constraint)

                        _ -> undefined)


            -- 1.1 Verify annotated types of each func param
            verifiedFuncParams <- verifyParamType symtab' funcParams verifyType

            -- 1.2 populate symbol table with function parameters
            symtab'' <- 
                populateSymbolTable 
                symtab' 
                verifiedFuncParams
                (\id expectedType -> 
                    case expectedType of
                        KeliTypeConstraint c ->
                            KeliSymType (KeliTypeParam id c)
                        _ -> 
                            KeliSymConst (KeliConst id (KeliTypeCheckedExpr (KeliId id) expectedType) Nothing))
            
            -- 2. verify return type
            verifiedReturnType <- verifyType symtab'' returnType


            -- 3. check if user is declaring generic type (a.k.a type constructor)
            if verifiedReturnType `typeEquals` KeliTypeType then
                -- 3.1 make sure every param has the type of TypeConstraint
                case find (\(_,paramType) -> not (case paramType of KeliTypeConstraint _ -> True; _ -> False)) verifiedFuncParams of
                    Just p -> 
                        Left (KErrorInvalidTypeConstructorParam p)
                    Nothing ->
                        -- insert the name of this user-defined type into the symbol table, this is necessary for recursive types to be analyzed properly
                        let typeId = concat (map snd funcIds) in
                        undefined
                        -- let symtab''' = symtab'' |> (typeId, KeliSymType (KeliTypeTemporaryAliasForRecursiveType funcIds (length funcParams))) in
                        -- case convertExprToSymbol symtab''' funcBody funcIds of
                        --     Right (Right symbols) ->
                        --         Right symbols
                                
                        --     Right (Left expr) ->
                        --         Left (KErrorBodyOfGenericTypeIsNotTypeDeclaration expr)

                        --     Left err ->
                        --         Left err
            else do
                -- 3. type check the function body
                typeCheckedBody <- typeCheckExpr symtab'' funcBody
                let bodyType = getType typeCheckedBody
                let result = Right [KeliSymFunc [func {
                                        funcDeclGenericParams = verifiedGenericParams,
                                        funcDeclBody = typeCheckedBody,
                                        funcDeclParams = verifiedFuncParams,
                                        funcDeclReturnType = verifiedReturnType
                                    }]]

                -- 4. ensure body type adheres to return type
                if bodyType `typeEquals` verifiedReturnType then
                    result
                else 
                    case bodyType of
                        KeliTypeSingleton (_,"undefined") -> result
                        _ -> Left (KErrorUnmatchingFuncReturnType (getType typeCheckedBody) verifiedReturnType)
                    
    
    KeliIdlessDecl expr -> do
        checkedExpr <- typeCheckExpr symtab expr
        Right [KeliSymInlineExprs [checkedExpr]]

    KeliTypeAliasDecl name (KeliTypeTagUnion tags) -> do
        verifiedTags <- 
            mapM
            (\tag -> case tag of
                KeliTagCarryless name _ -> 
                    Right (KeliTagCarryless name KeliTypeUndefined)
                KeliTagCarryful name carryType _ -> do
                    verifiedCarryType <- verifyType symtab carryType
                    Right (KeliTagCarryful name verifiedCarryType KeliTypeUndefined))
            tags

        let 
            -- circular structure. Refer https://wiki.haskell.org/Tying_the_Knot
            tagUnionType = KeliTypeAlias name (KeliTypeTagUnion tags') 
            tags' =
                map 
                    (\x -> case x of
                        KeliTagCarryless tag _          -> (KeliTagCarryless tag tagUnionType)
                        KeliTagCarryful tag carryType _ -> (KeliTagCarryful tag carryType tagUnionType)) 
                    verifiedTags
                    in
            Right ([KeliSymType tagUnionType] ++ (map KeliSymTag tags') :: [KeliSymbol])
    
    KeliTypeAliasDecl name t -> do
        verifiedType <- verifyType symtab t
        Right [KeliSymType (KeliTypeAlias name verifiedType)]

    other -> error (show other)

typeCheckExpr :: KeliSymTab -> KeliExpr -> Either KeliError KeliExpr
typeCheckExpr symtab e = 
    case typeCheckExpr' symtab e of 
        expr@(Right KeliTypeCheckedExpr{}) -> 
            expr

        expr@(Right _) -> 
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
                error (show other)


    -- NOTES:
    -- * Why isn't func call params type checked at the very beginning?
    --      Because we need to cater for the magic `_.tag XXX` and `record.XXX YYY` function
    --      That's why you will see a lot of duplicated code for typeCheckExprs
    KeliFuncCall params funcIds ref -> do
        typeCheckedParams <- typeCheckExprs symtab params

        -- Check if user are calling record getter/setter function
        let subject = typeCheckedParams !! 0 in
            case unpackType (getType subject) of
                KeliTypeRecord kvs ->  
                    let funcId = intercalate "$" (map snd funcIds) in
                    case find (\((_,key),_) -> key == funcId) kvs of
                        Just (token, expectedType) -> 
                            -- Check if is getter or setter
                            if length (tail typeCheckedParams) == 0
                            then -- is getter
                                Right (KeliTypeCheckedExpr (KeliRecordGetter subject token) expectedType) 
                            else -- is setter
                                -- check is setter value has the correct type
                                let [newValue] = tail typeCheckedParams in
                                if getType newValue `typeEquals` expectedType then 
                                    Right (KeliTypeCheckedExpr (KeliRecordSetter subject token newValue) expectedType) 
                                else
                                    Left KErrorWrongTypeInSetter

                        Nothing -> 
                            typeCheckFuncCall symtab typeCheckedParams funcIds

                _ -> 
                    typeCheckFuncCall symtab typeCheckedParams funcIds

    KeliRecord kvs -> do
        let keys = map fst kvs 
        let values = map snd kvs 
        typeCheckedExprs <- typeCheckExprs symtab values
        return 
            (KeliTypeCheckedExpr 
                (KeliRecord (zip keys typeCheckedExprs))
                (KeliTypeRecord (zip keys (map getType typeCheckedExprs))))
    other -> 
        error (show other)

    where 
-- NOTE: params should be type checked using typeCheckExprs before passing into the typeCheckFuncCall function
typeCheckFuncCall :: KeliSymTab -> [KeliExpr] -> [StringToken] -> Either KeliError KeliExpr
typeCheckFuncCall symtab params funcIds = 
    let typeOfFirstParam = unpackType (getType (head params)) in
    case typeOfFirstParam of
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
                    typeCheckFuncCall' symtab params funcIds
                
                GotExcessive excessiveProps ->
                    Left (KErrorExcessiveProperties excessiveProps)
                
                Missing missingProps ->
                    Left (KErrorMissingProperties missingProps)
                
                PerfectMatch -> 
                    let expectedPropTypePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) propTypePairs in
                    let actualPropValuePairs = sortBy (\((_,a),_) ((_,b),_) -> compare a b) (zip actualProps values) in
                    case find (\(expected, actual) -> 
                            let expectedType = snd expected in
                            let actualType = getType (snd actual) in
                            not (expectedType `typeEquals` actualType)
                        ) (zip expectedPropTypePairs actualPropValuePairs) of
                        Just (expected, actual) -> 
                            Left (KErrorPropretyTypeMismatch (fst expected) (snd expected) (snd actual))
                        Nothing -> 
                            Right (KeliTypeCheckedExpr (KeliRecord actualPropValuePairs) (KeliTypeRecord propTypePairs))

                        
                

        -- (B) check if user is invoking carryful tag constructor
        KeliTypeCarryfulTagConstructor tag carryType belongingType -> 
            if length funcIds == 1 && (snd (funcIds !! 0)) == "carry" then
                let carryExpr     = params !! 1 in
                let carryExprType = getType carryExpr in
                let carryType'    = carryType in
                if carryExprType `typeEquals` carryType' then
                    Right (KeliTypeCheckedExpr (KeliTagConstructor tag (Just carryExpr)) belongingType)
                else
                    Left (KErrorIncorrectCarryType carryType' carryExpr)
            else
                -- treat as regular function call
                typeCheckFuncCall' symtab params funcIds
            
        
        -- (C) check if user is calling tag matchers
        KeliTypeTagUnion tags -> 
            let branches = tail params in
            let firstBranch = head branches in
            let subject = head params in
            let tagsWithQuestionMark = map 
                    (\(pos,id) -> (pos,id ++ "?")) 
                    (map 
                        (\tag -> case tag of
                            KeliTagCarryful id _ _ -> id
                            KeliTagCarryless id _  -> id) 

                        tags) in
            case match funcIds tagsWithQuestionMark of
                GotDuplicates -> 
                    Left (KErrorDuplicatedTags funcIds)

                ZeroIntersection -> 
                    -- treat as regular function call
                    typeCheckFuncCall' symtab params funcIds
                
                GotExcessive excessiveCases ->
                    Left (KErrorExcessiveTags excessiveCases)
                
                Missing cases ->
                    if "else?" `elem` (map snd funcIds) then
                        let tagBranches = zip funcIds branches in
                        let elseBranch = fromJust (find (\((_,id),_) -> id == "else?") tagBranches) in
                        let otherBranches = filter (\((_,id),_) -> id /= "else?") tagBranches in

                        Right 
                            (KeliTypeCheckedExpr 
                                (KeliTagMatcher 
                                    subject 
                                    otherBranches (Just (snd elseBranch))) (getType (head branches)))

                    else -- missing tags
                        Left (KErrorMissingTags cases)

                PerfectMatch ->
                    -- check if all branch have the same type
                    if any (\x -> not (getType x `typeEquals` getType firstBranch)) branches then
                        Left (KErrorNotAllBranchHaveTheSameType (zip funcIds branches))
                    else
                        -- complete tag matches
                        Right (KeliTypeCheckedExpr (KeliTagMatcher subject (zip funcIds branches) Nothing) (getType (head branches)))

        _ ->
            -- treat as regular function call
            typeCheckFuncCall' symtab params funcIds
            
typeCheckFuncCall' :: KeliSymTab -> [KeliExpr] -> [StringToken] -> Either KeliError KeliExpr
typeCheckFuncCall' symtab funcCallParams funcIds = 
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
verifyTypeConstraint symtab expr = undefined
    -- case expr of 
    --     (KeliId token@(_,id)) ->
    --         case lookup id symtab of
    --             Just (KeliSymType constraint@(KeliTypeConstraint{})) -> Right constraint
    --             Just other -> Left (KErrorNotATypeConstraint other)
    --             Nothing    -> Left (KErrorUsingUndefinedId token)
        
    --     _ -> undefined

typeCheckExprs :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliExpr]
typeCheckExprs symtab exprs = mapM (typeCheckExpr symtab) exprs


getTypeWithoutResolvingAlias :: KeliExpr -> KeliType
getTypeWithoutResolvingAlias e = case e of
    KeliTypeCheckedExpr _ t -> t
    _ -> undefined
    
type KeliSymTab = OMap String KeliSymbol

emptyKeliSymTab :: KeliSymTab
emptyKeliSymTab = empty


class HaveType a where
    getType :: a -> KeliType

instance HaveType KeliExpr where
    getType (KeliTypeCheckedExpr _ exprType) = exprType
    getType e = KeliTypeUnverified e


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