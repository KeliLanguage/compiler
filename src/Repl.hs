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
import qualified Ast.Verified as V

keliRead :: IO String
keliRead 
    = putStr "keli > "
    >> hFlush stdout
    >> getLine

keliEval :: (Env, String) -> String -> Either [KeliError] (IO String, (Env, String))
keliEval (prevEnv, prevBytecode) input 
    =   keliParse "<repl>" input >>= 
        analyzeDecls'' prevEnv  >>= \(newEnv, decls) ->
        let newBytecodeToBeExecuted = keliTranspile decls in 
        
        let onlyDeclarationDecls = filter (\s -> case s of V.IdlessDecl {} -> False; _ -> True) decls in
        let newByteCodeToBePassFoward = keliTranspile onlyDeclarationDecls in
        Right (keliExecute (prevBytecode ++ newBytecodeToBeExecuted), (newEnv, newByteCodeToBePassFoward))
        
    where 
        analyzeDecls''
            :: Env -- previous env
            -> [Raw.Decl] -- parsed input
            -> Either [KeliError] (Env, [V.Decl]) -- (accumulatedErrors, newEnv, newDecls)
        analyzeDecls'' env decls = 
            let (errors, env', decls') = analyzeDecls [] env decls in
            if length errors > 0 then
                Left errors
            else 
                Right (env', decls')
        

keliPrint :: String -> IO ()
keliPrint = putStrLn

keliRepl' :: Env -> String -> IO ()
keliRepl' prevEnv prevBytecode = do
    input <- keliRead
    unless (input == ":quit") 
        (case keliEval (prevEnv, prevBytecode) input of
            Right (evaluatedOutput, (newEnv,newBytecode)) -> 
                evaluatedOutput >>= keliPrint >> keliRepl' newEnv (prevBytecode ++ newBytecode)
            Left err ->
                keliPrint (show err) >> keliRepl' prevEnv prevBytecode) 



    

keliRepl :: IO ()
keliRepl = keliRepl' emptyEnv ""