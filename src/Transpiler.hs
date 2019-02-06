module Transpiler 
where

import Prelude hiding (id)
import Data.List
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)


import qualified Ast.Verified as V
import Symbol

keliTranspile :: [KeliSymbol] -> String
keliTranspile symbols = (intercalate ";\n" (map transpile symbols)) ++ ";\n"


class Transpilable a where
    transpile :: a -> String

idPrefix :: String
idPrefix = "_"

instance Transpilable V.Tag where
    transpile tag = case tag of 
        V.CarrylessTag (_,id) _ -> 
            idPrefix ++ id ++ ":({__tag:\"" ++ id ++ "\"})"

        V.CarryfulTag (_,id) _ _ -> 
            idPrefix ++ id ++ ":(_carry)=>({__tag:\"" ++ id ++ "\",_carry})"


instance Transpilable KeliSymbol where
    transpile symbol = case symbol of
        KeliSymFunc fs ->
            intercalate ";" (map transpile fs)

        KeliSymConst (_,id) expr ->
            "const " ++ idPrefix ++ id ++ "=" ++ transpile expr

        KeliSymType (V.TypeAlias _ (V.ConcreteType (V.TypeTaggedUnion (V.TaggedUnion (_,id) ids tags _)))) ->
            "const " ++ idPrefix ++ id ++ "={" ++ intercalate "," (map transpile tags) ++ "}"

        KeliSymTypeConstructor (V.TaggedUnion name _ tags _) ->
            "const " ++ idPrefix ++ snd name ++ "={" ++ intercalate "," (map transpile tags) ++ "}"

        KeliSymTypeConstructor {} ->
            ""

        KeliSymType {} -> 
            ""

        KeliSymTypeConstraint {} ->
            ""

        KeliSymInlineExprs exprs -> 
            intercalate ";" (map (\x -> "console.log(" ++ transpile x ++ ")") exprs)

        other ->
            error (show other)

joinIds :: [V.StringToken] -> String
joinIds ids = intercalate "_" (map snd ids)

instance Transpilable V.Decl where
    transpile x = case x of 
        V.ConstDecl c  -> transpile c
        V.IdlessDecl e -> transpile e
        V.FuncDecl f   -> transpile f


instance Transpilable V.Func where
    transpile f@(V.Func _ params _ _ body) 
        = let params' = intercalate "," (map ((idPrefix ++ ) . snd . fst) params) in
        "function " ++ fst (V.getIdentifier f) ++ "(" ++ params' ++ "){return " ++ transpile body ++ ";}"



instance Transpilable V.Const where
    transpile (V.Const (_,id) expr)
        = "const " ++ idPrefix ++ id ++ "=" ++ (transpile expr)

instance Transpilable V.Expr where
    transpile expr = case expr of 
        V.Expr(V.IntExpr (_,value)) _                       
            -> show value

        V.Expr(V.DoubleExpr (_, value)) _                   
            -> show value

        V.Expr(V.StringExpr (_,value)) _
            -> show value

        V.Expr(V.Id     (_,value)) _
            -> idPrefix ++ value

        V.Expr(V.Lambda params body) _                      
            -> "(" ++ intercalate "," (map snd params) ++ ")=>(" ++ transpile body ++ ")"

        V.Expr(V.Record kvs) _                              
            -> transpileKeyValuePairs False (kvs)

        V.Expr(V.RecordGetter expr prop) _                  
            -> transpile expr ++ "." ++ snd prop

        V.Expr(V.RecordSetter subject prop newValue) _      
            -> "({...(" ++ transpile subject ++ ")," ++ snd prop ++ ":(" ++ transpile newValue ++ ")})"

        V.Expr(V.TagMatcher subject branches elseBranch) _
            -> 
            -- We will need to implement lazy evaluation here, as JavaScript is strict
            -- Also, lazy evaluation is needed to prevent evaluating unentered branch
            "((" ++ transpileKeyValuePairs True branches ++ "[" ++ transpile subject ++ ".__tag + '?'])" ++ 
                (case elseBranch of
                    Just expr -> " || " ++ "(" ++ (lazify (transpile expr)) ++ ")"
                    Nothing   -> "") ++ ")()"

        V.Expr(V.FuncCall params _ ref) _ -> 
            fst (V.getIdentifier ref) ++ "(" ++ intercalate "," (map transpile params) ++")"

        V.Expr(V.FFIJavascript (_,code)) _ ->
            code

        V.Expr(V.RetrieveCarryExpr expr) _ ->
            "((" ++ transpile expr ++ ")._carry)"

        V.Expr 
            (V.CarryfulTagExpr (_,tag) carry)  
            (V.ConcreteType (V.TypeTaggedUnion (V.TaggedUnion (_,id) _ _ _)))
                -> idPrefix ++ id ++ "." ++ idPrefix ++ tag ++ "("++ transpile carry ++")"

        V.Expr 
            (V.CarrylessTagConstructor(_,tag) _)
            (V.ConcreteType (V.TypeTaggedUnion (V.TaggedUnion(_,id) _ _ _)))
                -> idPrefix ++ id ++ "." ++ idPrefix ++ tag

        other -> 
            error (show other)

    

transpileKeyValuePairs :: Bool -> [(V.StringToken, V.Expr)] -> String
transpileKeyValuePairs lazifyExpr kvs 
    = "({" ++ (foldl' (\acc (key,expr) -> acc ++ (show (snd key)) ++ ":" 
        ++ (if lazifyExpr then lazify (transpile expr) else (transpile expr))
        ++ ",") "" kvs) ++ "})"


lazify :: String -> String
lazify str = "()=>(" ++ str ++ ")"
