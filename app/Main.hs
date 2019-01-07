module Main where

import Lib
import Transpiler
import Parser
import Compiler
import Analyzer
import SymbolTable

main :: IO ()
main = print "hello world"

keli filename = do
    contents <- readFile filename
    -- putStrLn (show contents)
    let ast = parseKeli contents
    return ast
    -- let table = (case ast of 
    --                 Right ast -> buildDeclTable ast
    --                 Left err  -> (error $ show err))
    -- return table

