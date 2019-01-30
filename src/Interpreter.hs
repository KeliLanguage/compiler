module Interpreter where

import StaticError
import Compiler
import System.Process


getBaseCode :: IO String 
getBaseCode = readFile "./kelilib/base.keli"


keliInterpret :: String -> String -> IO (Either [KeliError] String)
keliInterpret filename contents = do
    baseCode <- getBaseCode
    -- baseCode is loaded automaticall by default
    case (keliCompile filename $ baseCode ++ contents) of 
        Right code -> do 
            -- pPrint code
            output <- keliExecute code
            return (Right output)

        Left errs -> return (Left errs)

keliExecute :: String -> IO String
keliExecute code = readProcess "node" ["-e", code] []