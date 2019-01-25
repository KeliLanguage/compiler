{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- for exhasutive pattern checking
module Main where

import Repl
import Cli

main :: IO ()
main = keliRepl
