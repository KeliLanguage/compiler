module Keli where

import Transpiler
import Parser
import Analyzer
import SymbolTable
import Debug.Trace
import Debug.Pretty.Simple (pTraceShowId)
import Text.Pretty.Simple (pPrint)
import Data.List
import System.Process
import Data.Map.Ordered ((|>), assocs, member, lookup)

keli filename = do
    contents <- readFile filename
    keli' contents 

keli' contents = do
    case (keli'' contents) of
        Right code -> callCommand ("node -e " ++ pTraceShowId(show code))
        Left err -> pPrint err
    

keli'' contents
    =   parseKeli contents
    >>= buildSymTab 
    >>= analyze     
    >>= \symbols -> return (intercalate ";" (map transpile symbols))