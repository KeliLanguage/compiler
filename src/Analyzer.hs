{-# LANGUAGE MultiParamTypeClasses #-}
module Analyzer where

import Ast
import SymbolTable
import qualified Data.HashMap.Strict as H
import Data.List 
import Text.Parsec.Pos
import StaticError
import Debug.Trace
import Control.Monad

analyze :: KeliSymTab -> Either KeliError KeliSymTab
analyze symtab = 
    let symbols = H.elems symtab in 
    let constSymbols = filter (\x -> case x of KeliSymConst _ -> True; _->False) symbols in
    case analyzeSymbols constSymbols symtab of
        Right symtab' -> 
            let funcSymbols = filter (\x -> case x of KeliSymFunc _ -> True; _->False) symbols in
                analyzeSymbols funcSymbols symtab'
        Left err -> Left err


analyzeSymbols :: [KeliSym] -> KeliSymTab -> Either KeliError KeliSymTab
analyzeSymbols symbols symtab = 
    foldM
    ((\symtab' next -> do
        analyzedSymbol <- analyzeSymbol next symtab' 
        let (_,key) = getIdentifier next
        return (H.insert key analyzedSymbol symtab'))::KeliSymTab -> KeliSym -> Either KeliError KeliSymTab)
    symtab
    symbols

analyzeSymbol :: KeliSym -> KeliSymTab -> Either KeliError KeliSym
analyzeSymbol symbol symtab = case symbol of
    KeliSymConst KeliConst {
        constDeclId=id,
        constDeclValue=expr,
        constDeclType=expectedType
    } -> do 
            checkedExpr <- typeCheckExpr symtab expr
            case checkedExpr of
                KeliTypeExpr t -> return (KeliSymType t)
                _              -> return (KeliSymConst (KeliConst id checkedExpr expectedType))

    KeliSymFunc (func@KeliFunc {
        funcDeclParams=params,
        funcDeclReturnType=returnType,
        funcDeclBody=body
    }) -> do 
            symtab' <- 
                foldM 
                ((\acc (KeliFuncDeclParam id expectedType) -> 
                    let id' = snd id in
                    if H.member id' acc 
                        then Left (KErrorDuplicatedId id)
                        else Right(
                            H.insert 
                                id' 
                                (KeliSymConst 
                                    (KeliConst 
                                        id 
                                        (KeliTypeCheckedExpr 
                                            (KeliId id) 
                                            expectedType)
                                        Nothing)) 
                                acc))
                        ::KeliSymTab -> KeliFuncDeclParam -> Either KeliError KeliSymTab)
                symtab
                params
            typeCheckedBody <- typeCheckExpr symtab' body
            if getType typeCheckedBody == returnType then
                Right (KeliSymFunc (func {funcDeclBody = typeCheckedBody}))
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
        -> (case H.lookup id symtab of 
                Just (KeliSymConst (KeliConst _ (KeliTypeCheckedExpr _ exprType) _))
                    -> Right (KeliTypeCheckedExpr expr exprType)
                _ 
                    -> Left (KErrorUsingUndefinedId token))

    (KeliFuncCall params ids)
        -> 
            case head params of
                KeliId token@(_,"record") 
                    ->  if tail params == [] then 
                            Left (KErrorIncorrectUsageOfRecord token)
                        else if isType symtab (params !! 1) then -- assume user want to declare a record type
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
                        let funcCall = KeliFuncCall typeCheckedParams ids in
                            case H.lookup (getFuncId funcCall) symtab of
                                Just (KeliSymFunc f) 
                                    -> Right (KeliTypeCheckedExpr funcCall (funcDeclReturnType f))
                                _ 
                                    -> Left KErrorUsingUndefinedFunc
    where 
        getFuncId (KeliFuncCall params ids) 
            = intercalate "$" (map snd ids) ++ intercalate "$" (map (toString . getType) params)


resolveType :: KeliSymTab -> KeliExpr -> Either KeliError KeliType
resolveType symtab expr =
    case expr of
        (KeliId (_,id)) 
            -> case H.lookup id symtab of
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
        return (typeChecked:acc))
    [] 
    exprs

resolveTypes :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliType]
resolveTypes symtab exprs =
    foldM 
    (\acc next -> do
        resolvedType <- resolveType symtab next 
        return (resolvedType:acc))
    [] 
    exprs

    
isType :: KeliSymTab -> KeliExpr -> Bool
isType symtab expr = 
    case expr of
        (KeliId (_,id)) 
            -> case H.lookup id symtab of
                Just (KeliSymType _) -> True
                Nothing              -> False
        -- In the future need to handle function call, e.g. `a.list`
        _ 
            -> False
