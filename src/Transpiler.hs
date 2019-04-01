{-# LANGUAGE BangPatterns #-}
module Transpiler 
where

import Prelude hiding (id)
import Data.List
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Text.ParserCombinators.Parsec.Pos
import Data.Char
import Diagnostics

import qualified Ast.Verified as V
import Module

prefix :: String -> String
prefix s = "k$" ++ s -- k means Keli, this is to prevent conflicts with other JS libraries

transpileModule 
    :: Bool -- isEntryFile
    -> Bool -- showLineNumber
    -> Module 
    -> String
transpileModule isEntryFile showLineNumber  (Module name _ importedModules _ decls) = 
    "const " 
        ++ prefix name 
        ++ "=(()=>{" 
            ++ transpiledModules 
            ++ transpiledDecls 
            ++ "return{" ++ exports ++ "}})();"
    where
        transpiledModules = 
            concatMap (transpileModule False showLineNumber) (importedModules)

        transpiledDecls = 
            let !decls' = 
                    if isEntryFile then 
                        decls 
                    else -- remove all idless decls, accoding to specification 
                        filter (\d -> case d of V.IdlessDecl _ -> False; _ -> True) decls in
            (intercalate ";" (map (transpile showLineNumber) (decls'))) ++ ";"

        exports = 
            intercalate "," (
                concatMap 
                    (\d -> 
                        case d of
                            V.ConstDecl (_,id) _ ->
                                [prefix id]
                            V.FuncDecl signature _ ->
                                [getFuncName signature]

                            V.TaggedUnionDecl (V.TaggedUnion (_,name') _ _ _) ->
                                [prefix name']

                            _ ->
                                [])
                    decls)



class Transpilable a where
    transpile 
        :: Bool -- show line number
        -> a 
        -> String

quote :: String -> String
quote s = "\"" ++ s ++ "\""

squareBracket :: String -> String
squareBracket s = "[" ++ s ++ "]"

instance Transpilable V.Tag where
    transpile _ tag = case tag of 
        V.CarrylessTag (_,id) (V.TaggedUnion (_,name) _ _ _) -> 
            quote id ++ ":({__union:\"" ++ name ++ "\",__tag:\"" ++ id ++ "\"})"

        V.CarryfulTag (_,id) _ (V.TaggedUnion (_,name) _ _ _) -> 
            quote id ++ ":(__carry)=>({__union:\"" ++ name ++ "\",__tag:\"" ++ id ++ "\",__carry})"

joinIds :: [V.StringToken] -> String
joinIds ids = intercalate "_" (map snd ids)

instance Transpilable V.Decl where
    transpile showLineNumber decl = 
        case decl of 
            V.ConstDecl (_, id) expr  -> 
                "const " ++ prefix id ++ "=" ++ (transpile False expr)

            V.IdlessDecl expr -> 
                let lineNumber = line (start (getRange expr)) in 
                if showLineNumber then
                    "console.log(" ++ "\"Line " ++ show (lineNumber + 1) ++ " = \"+" ++
                    "KELI$show(" ++ transpile False expr ++ "))" 
                else
                    "console.log(KELI$show(" ++ transpile False expr ++ "))" 

            V.FuncDecl signature body -> 
                transpile False signature ++ "(" ++ transpile showLineNumber body ++ ");"

            V.TaggedUnionDecl (V.TaggedUnion (_,id) _ tags _) ->
                "const " ++ prefix id ++ "={" ++ intercalate "," (map (transpile showLineNumber) tags) ++ "}"
            
            V.ObjectAliasDecl{} ->
                ""


instance Transpilable V.Scope where
    transpile _ scope = 
        case scope of
            V.FromCurrentScope ->
                ""

            V.FromImports modulename ->
                prefix modulename ++ "."


instance Transpilable V.FuncSignature where
    transpile _ f@(V.FuncSignature _ _ params _ _) = 
        let params' = intercalate "," (map (\((_,id),_) -> prefix id) params) in
        "const " ++ getFuncName f ++ "=(" ++ params' ++ ")=>"

instance Transpilable V.Expr where
    transpile _ expr = case expr of 
        V.Expr(V.IntExpr (_,value)) _                       
            -> show value

        V.Expr(V.DoubleExpr (_, value)) _                   
            -> show value

        V.Expr(V.StringExpr (_,value)) _
            -> show value

        V.Expr(V.GlobalId _ (_, id) scope) _
            -> transpile False scope ++ prefix id

        V.Expr(V.LocalId _ (_, id)) _
            -> prefix id

        V.Expr(V.Lambda ((_, paramId),_) body) _                      
            -> "(" ++ prefix paramId ++ ")=>(" ++ transpile False body ++ ")"

        V.Expr(V.Object kvs) _                              
            -> transpileKeyValuePairs False kvs

        V.Expr(V.ObjectGetter expr prop) _                  
            -> transpile False expr ++ "." ++  (snd prop)

        V.Expr(V.ObjectSetter subject prop newValue) _      
            -> "({...(" ++ transpile False subject ++ ")," ++ (snd prop) 
                ++ ":(" ++ transpile False newValue ++ ")})"

        V.Expr (V.ObjectLambdaSetter subject (_,prop) (_, lambdaParamId) lambdaBody) _
            -> 
                "({...(" ++ transpile False subject ++ ")," 
                ++ prop ++ ":("
                ++ "((" ++ prefix lambdaParamId ++ ")=>(" 
                ++ transpile False lambdaBody ++ "))"
                ++ "((" ++ transpile False subject ++ ")." ++ prop ++ ")"
                ++ ")})"


        V.Expr(V.TagMatcher subject branches elseBranch) _
            -> 
            -- We will need to implement lazy evaluation here, as JavaScript is strict
            -- Also, lazy evaluation is needed to prevent evaluating unentered branch
            "(($$=>({" ++ intercalate "," (map (transpile False) branches) 
                ++ "})[$$.__tag])(" ++ transpile False subject ++ ")" 
                ++ (case elseBranch of
                    Just expr' -> " || " ++ "(" ++ (lazify (transpile False expr')) ++ ")"
                    Nothing   -> "") ++ ")()"

        V.Expr(V.FuncCall params _ (scope,ref)) _ -> 
            transpile False scope ++ getFuncName ref ++ "(" ++ intercalate "," (map (transpile False) params) ++")"

        V.Expr(V.FFIJavascript (_,code)) _ ->
            code

        V.Expr 
            (V.CarryfulTagExpr (_,tag) carry scope)  
            ((V.TypeTaggedUnion (V.TaggedUnion (_,taggedUnionName) _ _ _)))
                -> transpile False scope 
                    ++ prefix taggedUnionName ++ squareBracket (quote tag) 
                    ++ "("++ transpile False carry ++")"

        V.Expr 
            (V.CarrylessTagExpr(_,tag) _ scope)
            ((V.TypeTaggedUnion (V.TaggedUnion (_,taggedUnionName) _ _ _)))
                -> transpile False scope ++ prefix taggedUnionName ++ squareBracket (quote tag)

        V.Expr (V.FuncApp f arg) _ ->
            transpile False f ++ "(" ++ transpile False arg ++ ")"


        V.Expr (V.Array exprs _) _ -> 
            "[" ++ intercalate "," (map (transpile False) exprs) ++ "]"

        other ->
            error (show other)

instance Transpilable V.TagBranch where
    transpile _ b = case b of
        V.CarrylessTagBranch (V.VerifiedTagname (_,tagname)) expr ->
            tagname ++ ":" ++ lazify (transpile False expr)

        V.CarryfulTagBranch (V.VerifiedTagname (_,tagname)) (_,binding) expr ->
            tagname ++ ":" ++ lazify( "((" ++ prefix binding ++ ")=>" ++ transpile False expr ++ ")($$.__carry)" )


transpileKeyValuePairs :: Bool -> [(V.StringToken, V.Expr)] -> String
transpileKeyValuePairs lazifyExpr kvs 
    = "({" ++ (foldl' (\acc (key,expr) -> acc ++ snd key ++ ":" 
        ++ (if lazifyExpr then lazify (transpile False expr) else (transpile False expr))
        ++ ",") "" kvs) ++ "})"


lazify :: String -> String
lazify str = "()=>(" ++ str ++ ")"

-- Each function identifier shall follows the following format:
--
--      <front part>$$<back part>
--      id1$id2$id3$$hash
--
--  where <front part> is function names and <back part> is the hash
--  hash is the line number of where the first funcId is defined
-- 
-- Example:
--      this:String.replace old:String with new:String | String = undefined
-- Shall have id of
--      replace$with$$1
--
-- This format is necessary, so that when we do function lookup,
--  we can still construct back the function details from its id when needed
--  especially when looking up generic functions
getFuncName :: V.FuncSignature -> String
getFuncName (V.FuncSignature{V.funcDeclIds=ids}) = 
    let hash = sourceLine (fst (head ids)) in
    intercalate "$" (map (toValidJavaScriptId . snd) ids) ++ "$$" ++ show hash

-- Basically, this function will convert all symbols to its corresponding ASCII code
-- e.g. toValidJavaScriptId "$" = "k$36"
toValidJavaScriptId :: String -> String
toValidJavaScriptId s = prefix (concatMap (\x -> if (not . isAlphaNum) x then show (ord x) else [x]) s)