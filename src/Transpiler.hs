module Transpiler 
where

import Prelude hiding (id)
import Data.List
import Debug.Trace
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

import Analyzer
import Ast
import Symbol

class Transpilable a where
    transpile :: a -> String

idPrefix :: String
idPrefix = "_"

instance Transpilable KeliSymbol where
    transpile x = case x of
        KeliSymFunc fs ->
            intercalate ";" (map transpile fs)

        KeliSymConst c ->
            transpile c

        KeliSymSingleton (_,id) ->
            "const " ++ idPrefix ++ id ++ "=null"

        KeliSymType _ -> 
            ""

        KeliSymTag (KeliTagCarryless (_,id) _) -> 
            "const " ++ idPrefix ++ id ++ "=({__tag:\"" ++ id ++ "\"})"

        KeliSymTag (KeliTagCarryful (_,id) _ _) -> 
            "const " ++ idPrefix ++ id ++ "=(_carry)=>({__tag:\"" ++ id ++ "\",_carry})"

        KeliSymInlineExprs exprs -> 
            intercalate ";" (map (\x -> "console.log(" ++ transpile x ++ ")") exprs)


instance Transpilable KeliDecl where
    transpile x = case x of 
        KeliConstDecl c  -> transpile c
        KeliIdlessDecl e -> transpile e
        KeliFuncDecl f   -> transpile f


instance Transpilable KeliFunc where
    transpile f@(KeliFunc _ params _ _ body) 
        = let params' = intercalate "," (map ((idPrefix ++ ) . snd . fst) params) in
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

        KeliFuncCall params _ (Just ref) -> snd (getIdentifier ref) ++ "(" ++ intercalate "," (map transpile params) ++")"
        _ -> undefined

    

transpileKeyValuePairs :: [(StringToken, KeliExpr)] -> String
transpileKeyValuePairs kvs 
    = "({" ++ (foldl' (\acc (key,expr) -> acc ++ (show (snd key)) ++ ":" ++ transpile expr ++ ",") "" kvs) ++ "})"



