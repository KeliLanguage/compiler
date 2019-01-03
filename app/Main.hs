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
    -- putStrLn (show contents)
    let ast = parseKeli contents
    putStrLn (show ast)
    let table = (case ast of 
                    Right ast -> buildDeclTable ast
                    Left err  -> (error $ show err))
    return table

