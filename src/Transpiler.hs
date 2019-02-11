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

prefix :: String -> String
prefix s = "$" ++ s

instance Transpilable V.Tag where
    transpile tag = case tag of 
        V.CarrylessTag (_,id) _ -> 
            prefix id ++ ":({__tag:\"" ++ id ++ "\"})"

        V.CarryfulTag (_,id) _ _ -> 
            prefix id ++ ":(__carry)=>({__tag:\"" ++ id ++ "\",__carry})"


instance Transpilable KeliSymbol where
    transpile symbol = case symbol of
        KeliSymFunc fs ->
            intercalate ";" (map transpile fs)

        KeliSymConst (_,id) expr ->
            "const " ++ prefix id ++ "=" ++ transpile expr

        KeliSymType (V.TypeAlias _ (V.ConcreteType (V.TypeTaggedUnion (V.TaggedUnion (_,id) ids tags _)))) ->
            "const " ++ prefix id ++ "={" ++ intercalate "," (map transpile tags) ++ "}"

        KeliSymTypeConstructor (V.TaggedUnion name _ tags _) ->
            "const " ++ prefix (snd name) ++ "={" ++ intercalate "," (map transpile tags) ++ "}"

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
        = let params' = intercalate "," (map ((prefix ) . snd . fst) params) in
        "function " ++ fst (V.getIdentifier f) ++ "(" ++ params' ++ "){return " ++ transpile body ++ ";}"



instance Transpilable V.Const where
    transpile (V.Const (_,id) expr)
        = "const " ++ prefix id ++ "=" ++ (transpile expr)

instance Transpilable V.Expr where
    transpile expr = case expr of 
        V.Expr(V.IntExpr (_,value)) _                       
            -> show value

        V.Expr(V.DoubleExpr (_, value)) _                   
            -> show value

        V.Expr(V.StringExpr (_,value)) _
            -> show value

        V.Expr(V.Id     (_,value)) _
            -> prefix value

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
            "(($$=>({" ++ intercalate "," (map transpile branches) ++ "})[$$.__tag])(" ++ transpile subject ++ ")" ++
                (case elseBranch of
                    Just expr -> " || " ++ "(" ++ (lazify (transpile expr)) ++ ")"
                    Nothing   -> "") ++ ")()"

        V.Expr(V.FuncCall params _ ref) _ -> 
            fst (V.getIdentifier ref) ++ "(" ++ intercalate "," (map transpile params) ++")"

        V.Expr(V.FFIJavascript (_,code)) _ ->
            code

        V.Expr(V.RetrieveCarryExpr expr) _ ->
            "((" ++ transpile expr ++ ").__carry)"

        V.Expr 
            (V.CarryfulTagExpr (_,tag) carry)  
            (V.ConcreteType (V.TypeTaggedUnion (V.TaggedUnion (_,id) _ _ _)))
                -> prefix id ++ "." ++ prefix tag ++ "("++ transpile carry ++")"

        V.Expr 
            (V.CarrylessTagConstructor(_,tag) _)
            (V.ConcreteType (V.TypeTaggedUnion (V.TaggedUnion(_,id) _ _ _)))
                -> prefix id ++ "." ++ prefix tag

        other -> 
            error (show other)

instance Transpilable V.Branch where
    transpile b = case b of
        V.CarrylessBranch (_,tagname) expr ->
            tagname ++ ":" ++ lazify (transpile expr)

        V.CarryfulBranch (_,tagname) (_,bindingVar) expr ->
            tagname ++ ":" ++ lazify ("(" ++ prefix bindingVar ++ "=>" ++ transpile expr ++ ")($$.__carry)")


transpileKeyValuePairs :: Bool -> [(V.StringToken, V.Expr)] -> String
transpileKeyValuePairs lazifyExpr kvs 
    = "({" ++ (foldl' (\acc (key,expr) -> acc ++ (show (snd key)) ++ ":" 
        ++ (if lazifyExpr then lazify (transpile expr) else (transpile expr))
        ++ ",") "" kvs) ++ "})"


lazify :: String -> String
lazify str = "()=>(" ++ str ++ ")"
