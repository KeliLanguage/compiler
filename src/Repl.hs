module Repl where

import System.IO
import Control.Monad
import Parser
import Analyzer
import Symbol
import Interpreter
import StaticError
import Transpiler

keliRead :: IO String
keliRead 
    = putStr "keli > "
    >> hFlush stdout
    >> getLine

keliEval :: (KeliSymTab, String) -> String -> Either KeliError (IO String, (KeliSymTab, String))
keliEval (prevSymtab, prevBytecode) input 
    =   keliParse input         >>= 
        analyzeDecls prevSymtab >>= \(newSymtab, symbols) ->
        let newBytecodeToBeExecuted = keliTranspile symbols in 
        
        let onlyDeclarationSymbols = filter (\s -> case s of KeliSymInlineExprs {} -> False; _ -> True) symbols in
        let newByteCodeToBePassFoward = keliTranspile onlyDeclarationSymbols in
        Right (keliExecute (prevBytecode ++ newBytecodeToBeExecuted), (newSymtab, newByteCodeToBePassFoward))
        
        
        

keliPrint :: String -> IO ()
keliPrint = putStrLn

keliRepl' :: KeliSymTab -> String -> IO ()
keliRepl' prevSymtab prevBytecode = do
    input <- keliRead
    unless (input == ":quit") 
        (case keliEval (prevSymtab, prevBytecode) input of
            Right (evaluatedOutput, (newSymtab,newBytecode)) -> 
                evaluatedOutput >>= keliPrint >> keliRepl' newSymtab (prevBytecode ++ newBytecode)
            Left err ->
                keliPrint (show err) >> keliRepl' prevSymtab prevBytecode) 


    

keliRepl :: IO ()
keliRepl = keliRepl' emptyKeliSymTab ""