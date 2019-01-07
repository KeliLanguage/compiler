module Transpiler 
where

import Ast
import Data.List

class Transpilable a where
    transpile :: a -> String

instance Transpilable KeliDecl where
    transpile x = case x of 
        KeliConstDecl (Just id) expr _ -> "const " ++ (snd id) ++ "=" ++ (transpile expr)
        KeliConstDecl Nothing   expr _ -> (transpile expr)

instance Transpilable KeliExpr where
    transpile x = case x of 
        KeliNumber (_,Left value)  -> show value
        KeliNumber (_,Right value) -> show value
        KeliString (_,value)       -> show value
        KeliId     (_,value)       -> value
        KeliLambda params body     -> "(" ++ intercalate "," (map snd params) ++ ")=>(" ++ transpile body ++ ")"
        KeliRecord kvs             -> transpileKeyValuePairs kvs
        KeliRecordGetter expr prop -> transpile expr ++ "." ++ prop
        KeliRecordSetter subject prop newValue  -> "({...(" ++ transpile subject ++ ")," ++ snd prop ++ ":(" ++ transpile newValue ++ ")})"
        KeliTagChecker subject branches         -> transpileKeyValuePairs branches ++ "[" ++ transpile subject ++ ".$tag]"
        KeliTagConstructor (_,tag) (Just carry) -> "({$tag:("++ tag ++"),carry:("++ transpile carry ++")})"
        KeliTagConstructor (_,tag) Nothing      -> "({$tag:("++ tag ++")})"

    

transpileKeyValuePairs :: [(StringToken, KeliExpr)] -> String
transpileKeyValuePairs kvs 
    = "({" ++ (foldl (\acc (key,expr) -> acc ++ (show (snd key)) ++ ":" ++ transpile expr) "" kvs) ++ "})"