module Compiler where

import Data.List

import StaticError
import Analyzer
import Parser
import Transpiler


keliCompile :: String -> Either KeliError String
keliCompile input
    =   keliParse input
    >>= analyze 
    >>= \symbols -> return (keliTranspile symbols)
