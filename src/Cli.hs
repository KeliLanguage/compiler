module Cli where

import Text.Pretty.Simple (pPrint)
import StaticError
import Interpreter


cli :: String -> IO (Either KeliError String)
cli filename = do
    contents <- readFile filename
    keliInterpret contents
