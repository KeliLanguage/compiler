module Keli where

import Transpiler
import Parser
import Analyzer
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Text.Pretty.Simple (pPrint)
import Data.List
import System.Process
import StaticError

getBaseCode :: IO String 
getBaseCode = readFile "./kelilib/base.keli"

keli :: String -> IO (Either KeliError String)
keli filename = do
    contents <- readFile filename
    keli' contents

keli' :: String -> IO (Either KeliError String)
keli' contents = do
    baseCode <- getBaseCode
    -- baseCode is loaded automaticall by default
    case (keli'' $ baseCode ++ contents) of 
        Right code -> do 
            -- pPrint code
            output <- readProcess "node" ["-e", code] []
            return (Right output)

        Left err -> return (Left err)
    
keli'' :: String -> Either KeliError String
keli'' contents
    =   parseKeli contents
    >>= analyze 
    >>= \symbols -> return (intercalate ";\n" (map transpile symbols))