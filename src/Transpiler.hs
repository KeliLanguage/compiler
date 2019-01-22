module Transpiler 
where

import Prelude hiding (id)
import Data.List
import Debug.Trace
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

import Analyzer
import qualified Ast.Raw as Raw
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

        KeliSymTag (Raw.TagCarryless (_,id) _) -> 
            "const " ++ idPrefix ++ id ++ "=({__tag:\"" ++ id ++ "\"})"

        KeliSymTag (Raw.TagCarryful (_,id) _ _) -> 
            "const " ++ idPrefix ++ id ++ "=(_carry)=>({__tag:\"" ++ id ++ "\",_carry})"

        KeliSymInlineExprs exprs -> 
            intercalate ";" (map (\x -> "console.log(" ++ transpile x ++ ")") exprs)


instance Transpilable Raw.Decl where
    transpile x = case x of 
        Raw.ConstDecl c  -> transpile c
        Raw.IdlessDecl e -> transpile e
        Raw.FuncDecl f   -> transpile f


instance Transpilable Raw.Func where
    transpile f@(Raw.Func _ params _ _ body) 
        = let params' = intercalate "," (map ((idPrefix ++ ) . snd . fst) params) in
        "function " ++ snd (Raw.getIdentifier f) ++ "(" ++ params' ++ "){return " ++ transpile body ++ ";}"



instance Transpilable Raw.Const where
    transpile (Raw.Const (_,id) expr _)
        = "const " ++ idPrefix ++ id ++ "=" ++ (transpile expr)

instance Transpilable Raw.Expr where
    transpile x = case x of 
        Raw.Number (_,Left value)                   -> show value
        Raw.Number (_,Right value)                  -> show value
        Raw.String (_,value)                        -> show value
        Raw.Id     (_,value)                        -> idPrefix ++ value
        Raw.Lambda params body                      -> "(" ++ intercalate "," (map snd params) ++ ")=>(" ++ transpile body ++ ")"
        Raw.Record kvs _                            -> transpileKeyValuePairs (kvs)
        Raw.RecordGetter expr prop                  -> transpile expr ++ "." ++ snd prop
        Raw.RecordSetter subject prop newValue _ _  -> "({...(" ++ transpile subject ++ ")," ++ snd prop ++ ":(" ++ transpile newValue ++ ")})"
        Raw.TagConstructor (_,tag) (Just carry)     -> idPrefix ++ tag ++ "("++ transpile carry ++")"
        Raw.TagConstructor (_,tag) Nothing          -> idPrefix ++ tag
        Raw.TypeCheckedExpr e _                     -> transpile e
        Raw.TagMatcher subject branches elseBranch        
            -> 
            "((" ++ transpileKeyValuePairs branches ++ "[" ++ transpile subject ++ ".__tag + '?'])" ++ 
                (case elseBranch of
                    Just expr -> " || " ++ transpile expr
                    Nothing   -> "") ++ ")"

        Raw.FuncCall params _ (Just ref) -> snd (Raw.getIdentifier ref) ++ "(" ++ intercalate "," (map transpile params) ++")"
        _ -> undefined

    

transpileKeyValuePairs :: [(Raw.StringToken, Raw.Expr)] -> String
transpileKeyValuePairs kvs 
    = "({" ++ (foldl' (\acc (key,expr) -> acc ++ (show (snd key)) ++ ":" ++ transpile expr ++ ",") "" kvs) ++ "})"



