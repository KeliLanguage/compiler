module Interpreter where

import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Data.List
import System.Process

import Compiler
import Transpiler
import Diagnostics
import PreludeJSCode
import qualified Data.HashMap.Strict as HashMap

keliInterpret ::  Bool -> String -> IO (Either String String) -- Left means Error, Right means Output
keliInterpret showLineNumber sourceFileName  = do
    contents <- readFile sourceFileName
    (errors, currentModule, _, _) <- keliCompile sourceFileName contents (HashMap.empty) []
    if length errors > 0 then
        let diagnostics = concatMap toDiagnostic errors in
        return (Left (intercalate "\n\n\n" (map renderDiagnostic diagnostics)))
    else do
        let code = transpileModule True showLineNumber currentModule
        output <- keliExecute (preludeJSCode ++ code)
        return (Right output)

renderDiagnostic :: Diagnostic -> String
renderDiagnostic d = 
    let position = start (range d) in
    "ERROR at " 
        ++ filename d 
        ++ ":" ++ show (line position + 1) 
        ++ ":" ++ show (character position + 1) 
        ++ "\n\n" ++ tabify (message d)

tabify :: String -> String
tabify str = intercalate "\n" (map (\s -> "    " ++ s) (lines str))

keliExecute :: String -> IO String
keliExecute code = readProcess "node" ["-e", code] []