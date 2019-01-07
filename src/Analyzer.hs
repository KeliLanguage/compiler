{-# LANGUAGE MultiParamTypeClasses #-}
module Analyzer where

import Ast
import SymbolTable
import qualified Data.HashMap.Strict as H
import Data.List 
import Text.Parsec.Pos
import StaticError

analzyeAst :: [KeliDecl] -> KeliSymTab -> ([KeliDecl], [KeliError])
analzyeAst decls declTable = foldl 
        (\acc next -> 
            let (prevDecls, errors1) = acc in 
            let (decl2, errors2)     = analyzeDecl next declTable in
            (decl2:prevDecls, errors1 ++ errors2))
        ([], [])
        decls

analyzeDecl :: KeliDecl -> KeliSymTab -> (KeliDecl, [KeliError])
analyzeDecl decl table = case decl of
    constDecl@(
        KeliConstDecl KeliConst {
            constDeclId=id,
            constDeclValue=expr,
            constDeclType=expectedType
        }) -> let (checkedExpr, errors) = typeCheckExpr table expr in
            ((KeliConstDecl (KeliConst id checkedExpr expectedType)), errors)
    KeliFuncDecl  {}  -> undefined

typeCheckExpr :: KeliSymTab -> KeliExpr  -> (KeliExpr, [KeliError])
typeCheckExpr table e = case e of 
    (expr@(KeliNumber(_,n))) 
        -> (KeliTypeCheckedExpr expr (case n of Left _ -> KeliTypeInt; Right _ -> KeliTypeFloat), [])
    (expr@(KeliString _))    
        -> (KeliTypeCheckedExpr expr KeliTypeString, [])
    (expr@(KeliTypeCheckedExpr _ _)) 
        -> (expr, [])
    (expr@(KeliId (_,id)))
        -> (case H.lookup id table of 
                Just (KeliSymConst (KeliConst _ (KeliTypeCheckedExpr _ exprType) _))
                    -> (KeliTypeCheckedExpr expr exprType, [])
                _ 
                    -> (expr, [KErrorUsingUndefinedId]))
    (KeliFuncCall params ids)
        -> 
            let (typeCheckedParams, errors) = typeCheckExprs table params in
            let funcCall = KeliFuncCall typeCheckedParams ids in 
            case H.lookup (getFuncId funcCall) table of
                Just (KeliSymFunc f) 
                    -> (KeliTypeCheckedExpr funcCall (funcDeclReturnType f), errors)
                _ 
                    -> (funcCall, errors++[KErrorUsingUndefinedFunc])
    where 
        getFuncId (KeliFuncCall params ids) 
            = intercalate "$" (map snd ids) ++ intercalate "$" (map (toString . getType) params)


typeCheckExprs :: KeliSymTab -> [KeliExpr] -> ([KeliExpr], [KeliError])
typeCheckExprs table exprs = 
        foldl (\(exprs1, errs1) (expr2, errs2) -> (expr2:exprs1, errs1 ++ errs2)) ([],[]) 
        (map (typeCheckExpr table) exprs)

    