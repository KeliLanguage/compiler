module Interpreter where

import StaticError
import Compiler
import System.Process
import StaticError
import Analyzer
import Parser
import Data.List
import Env
import Transpiler
import qualified Ast.Raw as Raw
import Diagnostics


getPreludeJs :: IO String 
getPreludeJs = readFile "/home/hou32hou/Repos/keli/compiler/kelilib/prelude.js"

keliInterpret :: String -> IO (Either String String) -- Left means Error, Right means Output
keliInterpret filename = do
    preludeJsCode <- getPreludeJs
    (errors, _, analyzedDecls) <- keliCompile filename
    if length errors > 0 then
        let diagnostics = concatMap toDiagnostic errors in
        return (Left (intercalate "\n" (map message diagnostics)))
    else do
        let code = keliTranspile analyzedDecls
        output <- keliExecute (preludeJsCode ++ code)
        return (Right output)


-- this function is used for testing only
-- it cannot deal with import statements
keliInterpretForTesting :: String -> String -> IO (Either [KeliError] String)
keliInterpretForTesting filename contents = do
    preludeJsCode <- getPreludeJs
    let parseResult = keliParse filename contents 
    case parseResult of
        Right rawDecls -> 
            let (errors, _, analyzedDecls) = analyze [initialEnv] rawDecls in
            if length errors > 0 then
                return (Left errors)
            else do
                let code = keliTranspile analyzedDecls
                -- putStrLn code
                output <- keliExecute (preludeJsCode ++ code)
                return (Right output)
        
        Left err ->
            return (Left err)


keliExecute :: String -> IO String
keliExecute code = readProcess "node" ["-e", code] []