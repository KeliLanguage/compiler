module Transpiler 
where

import Prelude hiding (id)
import Data.List
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

import qualified Ast.Verified as Verified
import Symbol

class Transpilable a where
    transpile :: a -> String

idPrefix :: String
idPrefix = "_"

instance Transpilable KeliSymbol where
    transpile symbol = case symbol of
        KeliSymFunc fs ->
            intercalate ";" (map transpile fs)

        KeliSymConst (_,id) expr ->
            "const " ++ idPrefix ++ id ++ "=" ++ transpile expr

        KeliSymType _ -> 
            ""

        KeliSymTypeConstraint {} ->
            ""

        KeliSymTag (Verified.CarrylessTag (_,id) _) -> 
            "const " ++ idPrefix ++ id ++ "=({__tag:\"" ++ id ++ "\"})"

        KeliSymTag (Verified.CarryfulTag (_,id) _ _) -> 
            "const " ++ idPrefix ++ id ++ "=(_carry)=>({__tag:\"" ++ id ++ "\",_carry})"

        KeliSymInlineExprs exprs -> 
            intercalate ";" (map (\x -> "console.log(" ++ transpile x ++ ")") exprs)

        other ->
            error (show other)


instance Transpilable Verified.Decl where
    transpile x = case x of 
        Verified.ConstDecl c  -> transpile c
        Verified.IdlessDecl e -> transpile e
        Verified.FuncDecl f   -> transpile f


instance Transpilable Verified.Func where
    transpile f@(Verified.Func _ params _ _ body) 
        = let params' = intercalate "," (map ((idPrefix ++ ) . snd . fst) params) in
        "function " ++ fst (Verified.getIdentifier f) ++ "(" ++ params' ++ "){return " ++ transpile body ++ ";}"



instance Transpilable Verified.Const where
    transpile (Verified.Const (_,id) expr)
        = "const " ++ idPrefix ++ id ++ "=" ++ (transpile expr)

instance Transpilable Verified.Expr where
    transpile (Verified.Expr x _) = case x of 
        Verified.IntExpr (_,value)                       -> show value
        Verified.DoubleExpr (_, value)                   -> show value
        Verified.StringExpr (_,value)                    -> show value
        Verified.Id     (_,value)                        -> idPrefix ++ value
        Verified.Lambda params body                      -> "(" ++ intercalate "," (map snd params) ++ ")=>(" ++ transpile body ++ ")"
        Verified.Record kvs                              -> transpileKeyValuePairs False (kvs)
        Verified.RecordGetter expr prop                  -> transpile expr ++ "." ++ snd prop
        Verified.RecordSetter subject prop newValue      -> "({...(" ++ transpile subject ++ ")," ++ snd prop ++ ":(" ++ transpile newValue ++ ")})"
        Verified.CarryfulTagExpr (_,tag) carry           -> idPrefix ++ tag ++ "("++ transpile carry ++")"
        Verified.CarrylessTagConstructor(_,tag)          -> idPrefix ++ tag
        Verified.TagMatcher subject branches elseBranch        
            -> 
            -- We will need to implement lazy evaluation here, as JavaScript is strict
            -- Also, lazy evaluation is needed to prevent evaluating unentered branch
            "((" ++ transpileKeyValuePairs True branches ++ "[" ++ transpile subject ++ ".__tag + '?'])" ++ 
                (case elseBranch of
                    Just expr -> " || " ++ "(" ++ (lazify (transpile expr)) ++ ")"
                    Nothing   -> "") ++ ")()"

        Verified.FuncCall params _ ref -> 
            fst (Verified.getIdentifier ref) ++ "(" ++ intercalate "," (map transpile params) ++")"

        Verified.FFIJavascript (_,code) ->
            code

        other -> (error (show other))

    

transpileKeyValuePairs :: Bool -> [(Verified.StringToken, Verified.Expr)] -> String
transpileKeyValuePairs lazifyExpr kvs 
    = "({" ++ (foldl' (\acc (key,expr) -> acc ++ (show (snd key)) ++ ":" 
        ++ (if lazifyExpr then lazify (transpile expr) else (transpile expr))
        ++ ",") "" kvs) ++ "})"


lazify :: String -> String
lazify str = "()=>(" ++ str ++ ")"
