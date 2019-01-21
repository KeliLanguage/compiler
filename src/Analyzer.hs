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
import Preprocess
import StaticError
import Symbol
import TypeCheck
import Util



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

            let populateSymbolTable = (\tempSymtab params constructor -> 
                    foldM ((\acc (id, expectedType) -> 
                    let id' = snd id in
                    if member id' acc then 
                        Left (KErrorDuplicatedId [id])
                    else 
                        Right(
                            let keyValue = (id', constructor id expectedType)
                            in acc |> keyValue
                                ))::KeliSymTab -> KeliFuncDeclParam -> Either KeliError KeliSymTab)
                    tempSymtab
                    params)

            -- 0.1 Verify annotated constraint of each generic param
            verifiedGenericParams <- verifyParamType symtab genericParams verifyTypeConstraint

            -- 0.2 populate symbol table with generic type parameters
            symtab2 <- 
                populateSymbolTable 
                symtab 
                verifiedGenericParams
                (\id paramType ->
                    case paramType of 
                        KeliTypeConstraint constraint -> 
                            KeliSymType (KeliTypeParam id constraint)

                        _ -> undefined)

            -- 1.1 Verify annotated types of each func param
            verifiedFuncParams <- verifyParamType symtab2 funcParams verifyType

            -- 1.2 populate symbol table with function parameters
            symtab3 <- 
                populateSymbolTable 
                symtab2 
                verifiedFuncParams
                (\id expectedType -> 
                    case expectedType of
                        KeliTypeConstraint c ->
                            KeliSymType (KeliTypeParam id c)
                        _ -> 
                            KeliSymConst (KeliConst id (KeliTypeCheckedExpr (KeliId id) expectedType) Nothing))
            
            -- 2. verify return type
            verifiedReturnType <- verifyType symtab3 returnType


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
                typeCheckedBody <- typeCheckExpr symtab3 funcBody
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

    other -> undefined
