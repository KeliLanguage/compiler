module Main where

import Lib
import Transpiler
import Parser
import Compiler
import Analyzer

main :: IO ()
main = print "hello world"

keli filename = do
    contents <- readFile filename
    let ast = parseKeli contents
    let table = (case ast of 
                    Right ast -> buildDeclTable ast
                    Left err  -> (error $ show err))
    return table

