module Preprocess where

import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered (OMap, (|>), assocs, member, lookup, empty, fromList) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)
import Data.Maybe (catMaybes, fromJust)

import Ast.Raw
import StaticError
import Symbol
import TypeCheck
import Util


-- preprocessDecls will apply magic constant/function (such as _.tag, record etc.) to the AST and transform it
-- NOTE: 
--      preprocessDecls will not do any type checking if not needed
--      Type checking shall only be perform in preprocessDecls if
--          (i)  it is crucial to differentiate if a expr is a type or expr (e.g. record)
--          (ii) it will reduce code duplication (e.g. type-checking record setter)
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
                        case preprocessExpr symtab AnalyzingExpr  value of 
                            Right (First expr) -> 
                                Right (KeliConstDecl (c {constDeclValue = expr}))

                            Right (Second type') ->
                                Right (KeliTypeAliasDecl [id] type')
                            
                            Right (Third tag) ->
                                Right (KeliTypeAliasDecl [id] (KeliTypeTagUnion [tag]))
                            
                            Left err -> Left err

        KeliFuncDecl f@(KeliFunc genParams funcParams ids returnType body) -> do
            let preprocessParams symtab' params = 
                    mapM 
                        (\(paramName, paramType) -> do 
                            preprocessedParamType <- preprocessType symtab' paramType
                            Right (paramName, preprocessedParamType)) 
                        params
        
            let populateSymtab symtab' params isGenericParam =
                    foldM 
                    (\tempSymtab (paramName, paramType) -> 
                        if member (snd paramName) tempSymtab then
                            Left (KErrorDuplicatedId [paramName])
                        else (
                            if isGenericParam then
                                Right 
                                    (tempSymtab |> (snd paramName, KeliSymType paramType))
                            else 
                                Right 
                                    (tempSymtab |> (snd paramName, KeliSymConst (KeliConst paramName (KeliId paramName) Nothing)))))
                    symtab'
                    params


            -- 1.1 preprocess generic params
            preprocessGenParams <- preprocessParams symtab genParams

            -- 1.2 populate symtab with generic params
            symtab2 <- populateSymtab symtab genParams True

            -- 2.1 preprocess func params
            preprocessedParams  <- preprocessParams symtab2 funcParams

            -- 2.2 populate symtab with func params
            symtab3 <- populateSymtab symtab2 funcParams False
                        

            -- 3. preprocess return type
            preprocessedReturnType <- preprocessType symtab3 returnType

            case preprocessExpr symtab3 AnalyzingExpr body of
                Right (First expr) ->
                    Right (KeliFuncDecl (f {
                        funcDeclGenericParams = preprocessGenParams,
                        funcDeclParams = preprocessedParams,
                        funcDeclBody = expr,
                        funcDeclReturnType = preprocessedReturnType
                    }))
                
                Right (Second type') ->
                    case type' of
                        KeliTypeAlias [(_,"undefined")] _ ->
                            Right (KeliFuncDecl f)

                        _ ->
                            Right (KeliTypeAliasDecl ids type')
                
                Right (Third tag) ->
                    undefined

                Left err -> Left err

        KeliIdlessDecl expr -> do
            result <- preprocessExpr symtab AnalyzingExpr expr
            case result of
                First preprocessedExpr ->
                    Right (KeliIdlessDecl preprocessedExpr)

                Second type' ->
                    Left (KErrorCannotDeclareTypeAsAnonymousConstant type')
                
                Third tag ->
                    Left (KErrorCannotDeclareTagAsAnonymousConstant tag)

preprocessType :: KeliSymTab -> KeliType -> Either KeliError KeliType
preprocessType symtab type' =
    case type' of
        KeliTypeUnverified expr -> 
            preprocessExpr symtab AnalyzingType expr >>= extractType 
             
        _ -> 
            Right type'

data OneOf3 a b c = First a | Second b | Third c deriving (Show)


-- Assumption is needed to determine whether :
        -- singleton is used as type or expr
        -- we are analyzing tag

data Assumption 
    = AnalyzingExpr 
    | AnalyzingType 
    deriving (Show)


preprocessExpr :: KeliSymTab -> Assumption -> KeliExpr -> Either KeliError (OneOf3 KeliExpr KeliType KeliTag)
preprocessExpr symtab assumption expr =
    case expr of
    KeliId id ->
        case lookup (snd id) symtab of
            Just (KeliSymConst (KeliConst _ value _)) -> 
                Right (First value)
            
            Just (KeliSymType t@(KeliTypeAlias _ (KeliTypeRecord kvs))) ->
                case assumption of 
                    AnalyzingExpr -> 
                        Right (First (KeliRecordConstructor kvs))

                    AnalyzingType ->
                        Right (Second t)

            
            Just (KeliSymType t) ->
                Right (Second t)
            
            Just (KeliSymTag tag) ->
                case tag of
                    KeliTagCarryless name belongingType ->
                        Right (First (KeliTypeCheckedExpr (KeliTagConstructor name Nothing) belongingType))
                    
                    KeliTagCarryful name carryType belongingType ->
                        Right (First (KeliTypeCheckedExpr (KeliId id) (KeliTypeCarryfulTagConstructor name carryType belongingType)))
            
            Just _ ->
                undefined
            
            Nothing ->
                Left (KErrorUsingUndefinedId id)

    KeliFuncCall params' funcIds ref -> do
        case head funcIds of 
            -- 0. Check if user wants to create a tagged union
            (_,"or") -> do
                let isTagOrUnion x = 
                        case x of 
                            Second (KeliTypeTagUnion{})-> True; 
                            Third _ -> True; 
                            _ -> False;
                params <- mapM (preprocessExpr symtab AnalyzingExpr) params';
                if isTagOrUnion (head params) then (
                    if length params /= 2 then
                        Left (KErrorIncorrectUsageOfTaggedUnion expr)
                    else if any (not . isTagOrUnion) params then
                        Left (KErrorIncorrectUsageOfTaggedUnion expr)
                    else
                        Right (Second (KeliTypeTagUnion (concat (map extractTag params)))))
                else  do
                    continuePreprocessFuncCall

            _ -> do  
                case (head params') of
                    (KeliId firstParamToken@(_,firstParamId)) -> 
                        case firstParamId of
                        -- 1. Check if user wants to create a tag
                        "_" ->
                            case funcIds !! 0 of 
                            (_,"tag") ->
                                if length params' < 2 then
                                    Left (KErrorIncorrectUsageOfTag expr)
                                else
                                    case params' !! 1 of
                                        KeliId tag ->
                                            -- 1.1 Check if user wants to create carryless/carryful tag
                                            if length funcIds < 2 then -- carryless tag
                                                Right (Third (KeliTagCarryless tag KeliTypeUndefined))

                                            else if snd (funcIds !! 1) == "carry" then do -- carryful tag
                                                if length params' < 3 then
                                                    Left (KErrorIncorrectUsageOfTag expr)
                                                else do
                                                    thirdParam <- preprocessExpr symtab AnalyzingType (params' !! 2)
                                                    case thirdParam of
                                                        Second carryType ->
                                                            Right (Third (KeliTagCarryful tag carryType KeliTypeUndefined))
                                                        
                                                        _ ->
                                                            Left (KErrorIncorrectUsageOfTag expr)
                                            else
                                                Left (KErrorIncorrectUsageOfTag expr)
                                        
                                        _ ->
                                            Left (KErrorIncorrectUsageOfTag expr)
                            _ ->
                                continuePreprocessFuncCall

                        -- 2. Check if the user wants to create a record (type/value)
                        "record" ->  
                            if length (tail params') == 0 then 
                                Left (KErrorIncorrectUsageOfRecord firstParamToken)

                            else do 
                                values <- mapM (preprocessExpr symtab AnalyzingType) (tail params')
                                case values !! 0 of
                                    -- assume user want to create a record value
                                    First _ -> do
                                        exprs <- mapM extractExpr values
                                        Right (First (KeliRecord (zip funcIds exprs) Nothing))
                                    
                                    -- assume user want to declare a record type
                                    Second _ -> do
                                        types <- mapM extractType values
                                        let keys = funcIds
                                        Right (Second (KeliTypeRecord (zip keys types)))
                            
                        _ -> 
                            continuePreprocessFuncCall

                    _ -> 
                        continuePreprocessFuncCall

        where 
            continuePreprocessFuncCall :: Either KeliError (OneOf3 KeliExpr KeliType KeliTag)
            continuePreprocessFuncCall = do
                firstParam <- preprocessExpr symtab AnalyzingExpr (head params') >>= extractExpr >>= (\x -> typeCheckExpr symtab x)

                let typeOfFirstParam = unpackType (getType firstParam) in
                    case typeOfFirstParam of
                    -- (A) check if user is invoking record constructor
                    KeliTypeRecordConstructor propTypePairs -> do
                        let expectedProps = map fst propTypePairs
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
                                values  <- mapM (preprocessExpr symtab AnalyzingExpr) (tail params') >>= mapM extractExpr 
                                Right (First (KeliRecord (zip actualProps values) (Just propTypePairs)))

                                    
                    -- (B) check if user is invoking carryful tag constructor
                    KeliTypeCarryfulTagConstructor tag carryType belongingType -> 
                        if length funcIds == 1 && (snd (funcIds !! 0)) == "carry" then do
                            carryExpr  <- preprocessExpr symtab AnalyzingExpr (params' !! 1)
                            carryExpr' <- extractExpr carryExpr
                            -- let carryExprType = getType carryExpr in
                            -- let carryType'    = carryType in
                            -- if carryExprType `typeEquals` carryType' then
                            Right (First (KeliTagConstructor tag (Just carryExpr')))
                            -- else
                            --     Left (KErrorIncorrectCarryType carryType' carryExpr)
                        else
                            treatAsNormalFuncCall
                        
                    
                    -- (C) check if user is calling tag matchers
                    KeliTypeTagUnion tags -> do
                        branches <- mapM (preprocessExpr symtab AnalyzingExpr) (tail params') >>= mapM extractExpr 
                        let firstBranch = head branches 
                        let subject = firstParam 
                        let tagsWithQuestionMark = map 
                                (\(pos,id) -> (pos,id ++ "?")) 
                                (map 
                                    (\tag -> case tag of
                                        KeliTagCarryful id _ _ -> id
                                        KeliTagCarryless id _  -> id) 
                                    tags)

                        case match funcIds tagsWithQuestionMark of
                            GotDuplicates -> 
                                Left (KErrorDuplicatedTags funcIds)

                            ZeroIntersection -> 
                                treatAsNormalFuncCall
                            
                            GotExcessive excessiveCases ->
                                Left (KErrorExcessiveTags excessiveCases)
                            
                            Missing cases ->
                                if "else?" `elem` (map snd funcIds) then
                                    let tagBranches = zip funcIds branches in
                                    let elseBranch = fromJust (find (\((_,id),_) -> id == "else?") tagBranches) in
                                    let otherBranches = filter (\((_,id),_) -> id /= "else?") tagBranches in
                                    Right (First (KeliTagMatcher subject otherBranches (Just (snd elseBranch))))
                                    -- Right (First 
                                    --     (KeliTypeCheckedExpr 
                                    --         (KeliTagMatcher 
                                    --             subject 
                                    --             otherBranches (Just (snd elseBranch))) (getType (head branches))))


                                else -- missing tags
                                    Left (KErrorMissingTags cases)

                            PerfectMatch ->
                                Right (First (KeliTagMatcher subject (zip funcIds branches) Nothing))


                    -- (D) check if user is calling record getter/setter
                    recordType@(KeliTypeRecord kvs) ->  
                        if length funcIds > 1 then
                            treatAsNormalFuncCall
                        else
                            let subject = firstParam in
                            case find (\((_,key),_) -> key == snd (funcIds !! 0)) kvs of
                                Just (token, expectedType) -> 
                                    -- Check if is getter or setter
                                    if length (tail params') == 0
                                    then -- is getter
                                        Right (First (KeliTypeCheckedExpr (KeliRecordGetter subject token) expectedType))
                                    else do -- is setter
                                        newValue <- preprocessExpr symtab AnalyzingExpr ((tail params') !! 0) >>= extractExpr
                                        Right (First (KeliRecordSetter subject token newValue expectedType recordType))

                                Nothing -> 
                                    treatAsNormalFuncCall

                    _ ->
                        treatAsNormalFuncCall
            
            treatAsNormalFuncCall = do
                params <- mapM (preprocessExpr symtab AnalyzingExpr) params';
                extractedExprs <- mapM extractExpr params
                Right (First (KeliFuncCall extractedExprs funcIds ref))

        
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