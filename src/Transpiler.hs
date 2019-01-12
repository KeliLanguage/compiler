module Transpiler 
where

import Prelude hiding (id)
import Ast
import SymbolTable
import Data.List
import Debug.Trace

class Transpilable a where
    transpile :: a -> String

idPrefix :: String
idPrefix = "_"

instance Transpilable KeliSym where
    transpile x = case x of
        KeliSymFunc f             -> transpile f
        KeliSymConst c            -> transpile c
        KeliSymSingleton (_,id)   -> "const " ++ idPrefix ++ id ++ "=null"
        KeliSymType             _ -> ""
        KeliSymTag (KeliTagCarryless (_,id) _) 
            -> "const " ++ idPrefix ++ id ++ "=({__tag:\"" ++ id ++ "\"})"
        KeliSymTag (KeliTagCarryful (_,id) _ _) 
            -> "const " ++ idPrefix ++ id ++ "=(_carry)=>({__tag:\"" ++ id ++ "\",_carry})"


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
        = "const " ++ idPrefix ++ id ++ "=" ++ (transpile expr)

instance Transpilable KeliExpr where
    transpile x = case x of 
        KeliNumber (_,Left value)               -> show value
        KeliNumber (_,Right value)              -> show value
        KeliString (_,value)                    -> show value
        KeliId     (_,value)                    -> idPrefix ++ value
        KeliLambda params body                  -> "(" ++ intercalate "," (map snd params) ++ ")=>(" ++ transpile body ++ ")"
        KeliRecord kvs                          -> transpileKeyValuePairs (kvs)
        KeliRecordGetter expr prop              -> transpile expr ++ "." ++ snd prop
        KeliRecordSetter subject prop newValue  -> "({...(" ++ transpile subject ++ ")," ++ snd prop ++ ":(" ++ transpile newValue ++ ")})"
        KeliTagConstructor (_,tag) (Just carry) -> idPrefix ++ tag ++ "("++ transpile carry ++")"
        KeliTagConstructor (_,tag) Nothing      -> idPrefix ++ tag
        KeliTypeCheckedExpr e _                 -> transpile e
        KeliTagMatcher subject branches elseBranch        
            -> 
            "((" ++ transpileKeyValuePairs branches ++ "[" ++ transpile subject ++ ".__tag + '?'])" ++ 
                (case elseBranch of
                    Just expr -> " || " ++ transpile expr
                    Nothing   -> "") ++ ")"

        _ -> undefined

    

transpileKeyValuePairs :: [(StringToken, KeliExpr)] -> String
transpileKeyValuePairs kvs 
    = "({" ++ (foldl (\acc (key,expr) -> acc ++ (show (snd key)) ++ ":" ++ transpile expr ++ ",") "" kvs) ++ "})"