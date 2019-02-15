module Repl where

import System.IO
import Control.Monad
import Parser
import Analyzer
import Env
import Interpreter
import StaticError
import Transpiler
import qualified Ast.Raw as Raw

keliRead :: IO String
keliRead 
    = putStr "keli > "
    >> hFlush stdout
    >> getLine

keliEval :: (Env, String) -> String -> Either [KeliError] (IO String, (Env, String))
keliEval (prevSymtab, prevBytecode) input 
    =   keliParse "<repl>" input >>= 
        analyzeDecls' prevSymtab  >>= \(newSymtab, symbols) ->
        let newBytecodeToBeExecuted = keliTranspile symbols in 
        
        let onlyDeclarationSymbols = filter (\s -> case s of KeliSymInlineExprs {} -> False; _ -> True) symbols in
        let newByteCodeToBePassFoward = keliTranspile onlyDeclarationSymbols in
        Right (keliExecute (prevBytecode ++ newBytecodeToBeExecuted), (newSymtab, newByteCodeToBePassFoward))
        
    where 
        analyzeDecls' 
            :: Env -- previous env
            -> [Raw.Decl] -- parsed input
            -> Either [KeliError] (Env, [KeliSymbol]) -- (accumulatedErrors, newSymtab, newSymbols)
        analyzeDecls' env decls = 
            let (errors, env', symbols') = analyzeDecls env decls in
            if length errors > 0 then
                Left errors
            else 
                Right (env', symbols')
        

keliPrint :: String -> IO ()
keliPrint = putStrLn

keliRepl' :: Env -> String -> IO ()
keliRepl' prevSymtab prevBytecode = do
    input <- keliRead
    unless (input == ":quit") 
        (case keliEval (prevSymtab, prevBytecode) input of
            Right (evaluatedOutput, (newSymtab,newBytecode)) -> 
                evaluatedOutput >>= keliPrint >> keliRepl' newSymtab (prevBytecode ++ newBytecode)
            Left err ->
                keliPrint (show err) >> keliRepl' prevSymtab prevBytecode) 



    

keliRepl :: IO ()
keliRepl = keliRepl' emptyEnv ""