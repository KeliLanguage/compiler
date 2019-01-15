module Keli where

import Transpiler
import Parser
import Analyzer
import Debug.Trace
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import Text.Pretty.Simple (pPrint)
import Data.List
import System.Process
import Data.Map.Ordered ((|>), assocs, member, lookup)

keli filename = do
    contents <- readFile filename
    keli' contents 

keli' contents = do
    case (keli'' contents) of
        Right code -> do 
            -- pPrint code
            callCommand ("node -e " ++ (show code))
        Left err -> error ("Error lol:" ++ (pTraceShow err $ ""))
    

keli'' contents
    =   parseKeli contents
    >>= analyze 
    >>= \symbols -> return (intercalate ";" (map transpile symbols))