module Interpreter where

import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Data.List
import System.Process

import Compiler
import Transpiler
import Diagnostics


getPreludeJs :: IO String 
getPreludeJs = readFile "/home/hou32hou/Repos/keli/compiler/kelilib/prelude.js"

keliInterpret ::  Bool -> String -> IO (Either String String) -- Left means Error, Right means Output
keliInterpret showLineNumber filename  = do
    preludeJsCode <- getPreludeJs
    contents <- readFile filename
    (errors, currentModule) <- keliCompile filename contents
    if length errors > 0 then
        let diagnostics = concatMap toDiagnostic errors in
        return (Left (intercalate "\n" (map message diagnostics)))
    else do
        let code = transpileModule True showLineNumber currentModule 
        output <- keliExecute (preludeJsCode ++ code)
        return (Right output)



keliExecute :: String -> IO String
keliExecute code = readProcess "node" ["-e", code] []