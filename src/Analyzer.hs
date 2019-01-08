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
    let result = analyzeSymbols constSymbols symtab in
    result


analyzeSymbols :: [KeliSym] -> KeliSymTab -> Either KeliError KeliSymTab
analyzeSymbols symbols symtab = 
        foldM
        ((\symtab' next -> do
            let key = getIdentifier next
            analyzedSymbol <- analyzeSymbol next symtab' 
            return (H.insert key analyzedSymbol symtab'))::KeliSymTab -> KeliSym -> Either KeliError KeliSymTab)
        symtab
        symbols

analyzeSymbol :: KeliSym -> KeliSymTab -> Either KeliError KeliSym
analyzeSymbol symbol table = case symbol of
    constDecl@(
        KeliSymConst KeliConst {
            constDeclId=id,
            constDeclValue=expr,
            constDeclType=expectedType
        }) -> do 
                checkedExpr <- typeCheckExpr table expr
                case checkedExpr of
                    KeliTypeExpr t -> return (KeliSymType t)
                    _              -> return (KeliSymConst (KeliConst id checkedExpr expectedType))

typeCheckExpr :: KeliSymTab -> KeliExpr  -> Either KeliError KeliExpr
typeCheckExpr table e = case e of 
    (expr@(KeliNumber(_,n))) 
        -> Right (KeliTypeCheckedExpr expr (case n of Left _ -> KeliTypeInt; Right _ -> KeliTypeFloat))

    (expr@(KeliString _))    
        -> Right (KeliTypeCheckedExpr expr KeliTypeString)

    (expr@(KeliTypeCheckedExpr _ _)) 
        -> Right expr

    (expr@(KeliId token@(_,id)))
        -> (case H.lookup id table of 
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
                        else if isType table (params !! 1) then -- assume user want to declare a record type
                            let types = tail params in do
                                resolvedTypes <- resolveTypes table types
                                return 
                                    (KeliTypeExpr
                                        (KeliTypeRecord (zip ids resolvedTypes)))


                        else -- assume user want to create an anonymous record
                            let values = tail params in do
                                typeCheckedExprs <- typeCheckExprs table values
                                return 
                                    (KeliTypeCheckedExpr 
                                        (KeliRecord (zip ids typeCheckedExprs)) 
                                        (KeliTypeRecord (zip ids (map getType typeCheckedExprs))))

                _ 
                    -> do
                        typeCheckedParams <- typeCheckExprs table params
                        let funcCall = KeliFuncCall typeCheckedParams ids in
                            case H.lookup (getFuncId funcCall) table of
                                Just (KeliSymFunc f) 
                                    -> Right (KeliTypeCheckedExpr funcCall (funcDeclReturnType f))
                                _ 
                                    -> Left KErrorUsingUndefinedFunc
    where 
        getFuncId (KeliFuncCall params ids) 
            = intercalate "$" (map snd ids) ++ intercalate "$" (map (toString . getType) params)


resolveType :: KeliSymTab -> KeliExpr -> Either KeliError KeliType
resolveType table expr =
    case expr of
        (KeliId (_,id)) 
            -> case H.lookup id table of
                Just (KeliSymType t) -> Right t
                Nothing              -> Left (KErrorUsingUndefinedType)
        -- In the future need to handle function call, e.g. `a.list`
        _ 
            -> Left (KErrorUsingUndefinedType)

typeCheckExprs :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliExpr]
typeCheckExprs table exprs = 
    foldM 
    (\acc next -> do
        typeChecked <- typeCheckExpr table next 
        return (typeChecked:acc))
    [] 
    exprs

resolveTypes :: KeliSymTab -> [KeliExpr] -> Either KeliError [KeliType]
resolveTypes table exprs =
    foldM 
    (\acc next -> do
        resolvedType <- resolveType table next 
        return (resolvedType:acc))
    [] 
    exprs

    
isType :: KeliSymTab -> KeliExpr -> Bool
isType table expr = 
    case expr of
        (KeliId (_,id)) 
            -> case H.lookup id table of
                Just (KeliSymType _) -> True
                Nothing              -> False
        -- In the future need to handle function call, e.g. `a.list`
        _ 
            -> False
