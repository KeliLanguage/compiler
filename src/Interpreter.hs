module Interpreter where

import StaticError
import Compiler
import System.Process


getPreludeJs :: IO String 
getPreludeJs = readFile "./kelilib/prelude.js"


keliInterpret :: String -> String -> IO (Either [KeliError] String)
keliInterpret filename contents = do
    preludeJsCode <- getPreludeJs
    case (keliCompile filename $ contents) of 

        Right code -> do 
            -- putStrLn code
            output <- keliExecute (preludeJsCode ++ code)
            return (Right output)

        Left errs -> return (Left errs)

keliExecute :: String -> IO String
keliExecute code = readProcess "node" ["-e", code] []