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

transpileModule :: Bool -> Module -> String
transpileModule isEntryFile (Module name importedModules _ decls) = 
    "const " 
        ++ prefix name 
        ++ "=(()=>{" ++ transpiledModules ++ transpiledDecls ++ "return{" ++ exports ++ "}})();"
    where
        transpiledModules = 
            concatMap (transpileModule False) importedModules

        transpiledDecls = 
            let decls' = 
                    if isEntryFile then 
                        decls 
                    else -- remove all idless decls, accoding to specification 
                        filter (\d -> case d of V.IdlessDecl _ -> False; _ -> True) decls in
            (intercalate ";" (map transpile decls')) ++ ";"

        exports = 
            intercalate "," (
                concatMap 
                    (\d -> 
                        case d of
                            V.ConstDecl (_,id) _ ->
                                [prefix id]
                            V.FuncDecl signature _ ->
                                [getFuncName signature]

                            V.TaggedUnionDecl (V.TaggedUnion (_,name) _ _ _) ->
                                [prefix name]

                            _ ->
                                [])
                    decls)



class Transpilable a where
    transpile :: a -> String

quote :: String -> String
quote s = "\"" ++ s ++ "\""

squareBracket :: String -> String
squareBracket s = "[" ++ s ++ "]"

instance Transpilable V.Tag where
    transpile tag = case tag of 
        V.CarrylessTag (_,id) _ -> 
            quote id ++ ":({__tag:\"" ++ id ++ "\"})"

        V.CarryfulTag (_,id) _ _ -> 
            quote id ++ ":(__carry)=>({__tag:\"" ++ id ++ "\",__carry})"

joinIds :: [V.StringToken] -> String
joinIds ids = intercalate "_" (map snd ids)

instance Transpilable V.Decl where
    transpile decl = 
        case decl of 
            V.ConstDecl (_, id) expr  -> 
                "const " ++ prefix id ++ "=" ++ (transpile expr)

            V.IdlessDecl expr -> 
                let lineNumber = line (start (getRange expr)) in 
                "console.log(" ++ "\"Line " ++ show (lineNumber + 1) ++ " = \"+" ++
                "KELI_PRELUDE$show(" ++ transpile expr ++ "))" 

            V.FuncDecl signature body -> 
                transpile signature ++ "(" ++ transpile body ++ ");"

            V.TaggedUnionDecl (V.TaggedUnion (_,id) _ tags _) ->
                "const " ++ prefix id ++ "={" ++ intercalate "," (map transpile tags) ++ "}"
            
            V.RecordAliasDecl{} ->
                ""


instance Transpilable V.Scope where
    transpile scope = 
        case scope of
            V.FromCurrentScope ->
                ""

            V.FromImports modulename ->
                prefix modulename ++ "."


instance Transpilable V.FuncSignature where
    transpile f@(V.FuncSignature _ _ params _ _) = 
        let params' = intercalate "," (map (\((_,id),_) -> prefix id) params) in
        "const " ++ getFuncName f ++ "=(" ++ params' ++ ")=>"

instance Transpilable V.Expr where
    transpile expr = case expr of 
        V.Expr(V.IntExpr (_,value)) _                       
            -> show value

        V.Expr(V.DoubleExpr (_, value)) _                   
            -> show value

        V.Expr(V.StringExpr (_,value)) _
            -> show value

        V.Expr(V.GlobalId _ (_, id) scope) _
            -> transpile scope ++ prefix id

        V.Expr(V.LocalId _ (_, id)) _
            -> prefix id

        V.Expr(V.Lambda ((_, paramId),_) body) _                      
            -> "(" ++ prefix paramId ++ ")=>(" ++ transpile body ++ ")"

        V.Expr(V.Record kvs) _                              
            -> transpileKeyValuePairs False kvs

        V.Expr(V.RecordGetter expr prop) _                  
            -> transpile expr ++ "." ++  (snd prop)

        V.Expr(V.RecordSetter subject prop newValue) _      
            -> "({...(" ++ transpile subject ++ ")," ++ (snd prop) 
                ++ ":(" ++ transpile newValue ++ ")})"

        V.Expr (V.RecordLambdaSetter subject (_,prop) (_, lambdaParamId) lambdaBody) _
            -> 
                "({...(" ++ transpile subject ++ ")," 
                ++ prop ++ ":("
                ++ "((" ++ prefix lambdaParamId ++ ")=>(" 
                ++ transpile lambdaBody ++ "))"
                ++ "((" ++ transpile subject ++ ")." ++ prop ++ ")"
                ++ ")})"


        V.Expr(V.TagMatcher subject branches elseBranch) _
            -> 
            -- We will need to implement lazy evaluation here, as JavaScript is strict
            -- Also, lazy evaluation is needed to prevent evaluating unentered branch
            "(($$=>({" ++ intercalate "," (map transpile branches) 
                ++ "})[$$.__tag])(" ++ transpile subject ++ ")" 
                ++ (case elseBranch of
                    Just expr' -> " || " ++ "(" ++ (lazify (transpile expr')) ++ ")"
                    Nothing   -> "") ++ ")()"

        V.Expr(V.FuncCall params _ (scope,ref)) _ -> 
            transpile scope ++ getFuncName ref ++ "(" ++ intercalate "," (map transpile params) ++")"

        V.Expr(V.FFIJavascript (_,code)) _ ->
            code

        V.Expr 
            (V.CarryfulTagExpr (_,tag) carry scope)  
            ( (V.TypeTaggedUnion (V.TaggedUnion (_,taggedUnionName) _ _ _)))
                -> transpile scope 
                    ++ prefix taggedUnionName ++ squareBracket (quote tag) 
                    ++ "("++ transpileKeyValuePairs False carry ++")"

        V.Expr 
            (V.CarrylessTagExpr(_,tag) _ scope)
            ( (V.TypeTaggedUnion (V.TaggedUnion (_,taggedUnionName) _ _ _)))
                -> transpile scope ++ prefix taggedUnionName ++ squareBracket (quote tag)

        V.Expr (V.FuncApp f arg) _ ->
            transpile f ++ "(" ++ transpile arg ++ ")"


        other -> 
            error (show other)

instance Transpilable V.TagBranch where
    transpile b = case b of
        V.CarrylessTagBranch (V.VerifiedTagname (_,tagname)) expr ->
            tagname ++ ":" ++ lazify (transpile expr)

        V.CarryfulTagBranch (V.VerifiedTagname (_,tagname)) propBindings expr ->
            -- Refer https://codeburst.io/renaming-destructured-variables-in-es6-807549754972
            tagname ++ ":" ++ lazify ("(({" 
                ++ (concatMap (\((_,from), (_,to), _) -> from ++ ":" ++ prefix to ++ ",") propBindings)
                ++ "})=>" 
                ++ transpile expr ++ ")($$.__carry)")


transpileKeyValuePairs :: Bool -> [(V.StringToken, V.Expr)] -> String
transpileKeyValuePairs lazifyExpr kvs 
    = "({" ++ (foldl' (\acc (key,expr) -> acc ++ snd key ++ ":" 
        ++ (if lazifyExpr then lazify (transpile expr) else (transpile expr))
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