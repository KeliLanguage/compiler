module Compiler where

import Data.List

import StaticError
import Analyzer
import Parser
import Transpiler


keliCompile :: String -> String -> Either [KeliError] String
keliCompile filename input
    =   keliParse filename input
    >>= analyze 
    >>= \decls -> return (keliTranspile decls)
