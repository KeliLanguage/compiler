module Interpreter where

import StaticError
import Compiler
import System.Process


getBaseCode :: IO String 
getBaseCode = readFile "./kelilib/base.keli"


keliInterpret :: String -> IO (Either KeliError String)
keliInterpret contents = do
    baseCode <- getBaseCode
    -- baseCode is loaded automaticall by default
    case (keliCompile $ baseCode ++ contents) of 
        Right code -> do 
            -- pPrint code
            output <- keliExecute code
            return (Right output)

        Left err -> return (Left err)

keliExecute :: String -> IO String
keliExecute code = readProcess "node" ["-e", code] []