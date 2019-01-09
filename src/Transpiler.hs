module Transpiler 
where

import Ast
import SymbolTable
import Data.List
import Debug.Trace

class Transpilable a where
    transpile :: a -> String

instance Transpilable KeliSym where
    transpile x = case x of
        KeliSymFunc f           -> transpile f
        KeliSymConst c          -> transpile c
        KeliSymSingleton (_,id) -> "const " ++ id ++ "=null;"
        KeliSymType             _ -> "";


instance Transpilable KeliDecl where
    transpile x = case x of 
        KeliConstDecl c  -> transpile c
        KeliIdlessDecl e -> transpile e
        KeliFuncDecl f   -> transpile f

instance Transpilable KeliFunc where
    transpile f@(KeliFunc _ params _ _ body) 
        = let params' = intercalate "," (map (snd . funcDeclParamId) params) in
        "function " ++ snd (getIdentifier f) ++ "(" ++ params' ++ "){return " ++ transpile body ++ ";}"


instance Transpilable KeliConst where
    transpile (KeliConst (_,id) expr _)
        = "const " ++ id ++ "=" ++ (transpile expr)

instance Transpilable KeliExpr where
    transpile x = case x of 
        KeliNumber (_,Left value)               -> show value
        KeliNumber (_,Right value)              -> show value
        KeliString (_,value)                    -> show value
        KeliId     (_,value)                    -> value
        KeliLambda params body                  -> "(" ++ intercalate "," (map snd params) ++ ")=>(" ++ transpile body ++ ")"
        KeliRecord kvs                          -> transpileKeyValuePairs (kvs)
        KeliRecordGetter expr prop              -> transpile expr ++ "." ++ snd prop
        KeliRecordSetter subject prop newValue  -> "({...(" ++ transpile subject ++ ")," ++ snd prop ++ ":(" ++ transpile newValue ++ ")})"
        KeliTagChecker subject branches         -> transpileKeyValuePairs branches ++ "[" ++ transpile subject ++ ".$tag]"
        KeliTagConstructor (_,tag) (Just carry) -> "({$tag:("++ tag ++"),carry:("++ transpile carry ++")})"
        KeliTagConstructor (_,tag) Nothing      -> "({$tag:("++ tag ++")})"
        KeliTypeCheckedExpr e _                 -> transpile e

    

transpileKeyValuePairs :: [(StringToken, KeliExpr)] -> String
transpileKeyValuePairs kvs 
    = "({" ++ (foldl (\acc (key,expr) -> acc ++ (show (snd key)) ++ ":" ++ transpile expr ++ ",") "" kvs) ++ "})"