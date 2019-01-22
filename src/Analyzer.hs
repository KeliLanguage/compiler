{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Analyzer where


import Control.Monad
import Data.List hiding (lookup)
import Data.Map.Ordered (OMap, (|>), assocs, member, lookup, empty, fromList) 
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Prelude hiding (lookup,id)
import Data.Maybe (catMaybes, fromJust)

import qualified Ast.Raw as Raw
import Preprocess
import StaticError
import Symbol
import TypeCheck
import Util



analyze :: [Raw.Decl] -> Either KeliError [KeliSymbol]
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


analyzeDecls :: [Raw.Decl] ->  Either KeliError KeliSymTab
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
                        let funcid = (intercalate "$" (map snd (Raw.funcDeclIds f))) in
                        let funcsWithSameName = lookup funcid symtab2 in
                        let funcParamTypes = (\func -> map snd (Raw.funcDeclParams func)) in
                        case funcsWithSameName of
                            Just (KeliSymFunc fs) ->
                                if any (\func -> all (\(t1,t2) -> t1 `Raw.typeEquals` t2) (zip (funcParamTypes f) (funcParamTypes func))) fs then
                                    Left (KErrorDuplicatedFunc f)
                                else
                                    Right (symtab2 |> (funcid, KeliSymFunc (f:fs)))
                            
                            Just _ ->
                                Left (KErrorDuplicatedId (Raw.funcDeclIds f))

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

                    KeliSymType (Raw.TypeSingleton _ ) -> -- do nothing
                        Right symtab2

                    _ -> 
                        let id@(_,key) = Raw.getIdentifier analyzedSymbol in
                        if member key symtab2 then
                            Left (KErrorDuplicatedId [id])
                        else 
                            Right (symtab2 |> (key, analyzedSymbol)))
            symtab1
            analyzedSymbols)
    )::KeliSymTab -> Raw.Decl -> Either KeliError KeliSymTab)
    emptyKeliSymTab
    decls

analyzeDecl :: Raw.Decl -> KeliSymTab -> Either KeliError [KeliSymbol]
analyzeDecl decl symtab = case decl of
    Raw.ConstDecl Raw.Const {
        Raw.constDeclId=id,
        Raw.constDeclValue=expr,
        Raw.constDeclType=expectedType
    } -> 
        case expr of 
            _ ->
                continueAnalyzeConstDecl

        where 
            continueAnalyzeConstDecl = do
                typeCheckedExpr <- typeCheckExpr symtab expr
                return [KeliSymConst (Raw.Const id typeCheckedExpr expectedType)]


    Raw.FuncDecl(func@Raw.Func {
        Raw.funcDeclGenericParams = genericParams,
        Raw.funcDeclIds           = funcIds,
        Raw.funcDeclParams        = funcParams,
        Raw.funcDeclReturnType    = returnType,
        Raw.funcDeclBody          = funcBody
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
                                ))::KeliSymTab -> Raw.FuncDeclParam -> Either KeliError KeliSymTab)
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
                        Raw.TypeConstraint constraint -> 
                            KeliSymType (Raw.TypeParam id constraint)

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
                        Raw.TypeConstraint c ->
                            KeliSymType (Raw.TypeParam id c)
                        _ -> 
                            KeliSymConst (Raw.Const id (Raw.TypeCheckedExpr (Raw.Id id) expectedType) Nothing))
            
            -- 2. verify return type
            verifiedReturnType <- verifyType symtab3 returnType


            -- 3. check if user is declaring generic type (a.k.a type constructor)
            if verifiedReturnType `Raw.typeEquals` Raw.TypeType then
                -- 3.1 make sure every param has the type of TypeConstraint
                case find (\(_,paramType) -> not (case paramType of Raw.TypeConstraint _ -> True; _ -> False)) verifiedFuncParams of
                    Just p -> 
                        Left (KErrorInvalidTypeConstructorParam p)
                    Nothing ->
                        -- insert the name of this user-defined type into the symbol table, this is necessary for recursive types to be analyzed properly
                        let typeId = concat (map snd funcIds) in
                        undefined
                        -- let symtab''' = symtab'' |> (typeId, KeliSymType (Raw.TypeTemporaryAliasForRecursiveType funcIds (length funcParams))) in
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
                                        Raw.funcDeclGenericParams = verifiedGenericParams,
                                        Raw.funcDeclBody = typeCheckedBody,
                                        Raw.funcDeclParams = verifiedFuncParams,
                                        Raw.funcDeclReturnType = verifiedReturnType
                                    }]]

                -- 4. ensure body type adheres to return type
                if bodyType `Raw.typeEquals` verifiedReturnType then
                    result
                else 
                    case bodyType of
                        Raw.TypeSingleton (_,"undefined") -> result
                        _ -> Left (KErrorUnmatchingFuncReturnType (getType typeCheckedBody) verifiedReturnType)
                    
    
    Raw.IdlessDecl expr -> do
        checkedExpr <- typeCheckExpr symtab expr
        Right [KeliSymInlineExprs [checkedExpr]]

    Raw.TypeAliasDecl name (Raw.TypeTagUnion tags) -> do
        verifiedTags <- 
            mapM
            (\tag -> case tag of
                Raw.TagCarryless name _ -> 
                    Right (Raw.TagCarryless name Raw.TypeUndefined)
                Raw.TagCarryful name carryType _ -> do
                    verifiedCarryType <- verifyType symtab carryType
                    Right (Raw.TagCarryful name verifiedCarryType Raw.TypeUndefined))
            tags

        let 
            -- circular structure. Refer https://wiki.haskell.org/Tying_the_Knot
            tagUnionType = Raw.TypeAlias name (Raw.TypeTagUnion tags') 
            tags' =
                map 
                    (\x -> case x of
                        Raw.TagCarryless tag _          -> (Raw.TagCarryless tag tagUnionType)
                        Raw.TagCarryful tag carryType _ -> (Raw.TagCarryful tag carryType tagUnionType)) 
                    verifiedTags
                    in
            Right ([KeliSymType tagUnionType] ++ (map KeliSymTag tags') :: [KeliSymbol])
    
    Raw.TypeAliasDecl name t -> do
        verifiedType <- verifyType symtab t
        Right [KeliSymType (Raw.TypeAlias name verifiedType)]

    other -> undefined
