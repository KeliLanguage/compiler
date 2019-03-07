{-# LANGUAGE BangPatterns #-}
module Cli where


import Options.Applicative
import Data.Semigroup ((<>))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import System.IO

import Interpreter
import Repl
import Compiler
import Diagnostics(toDiagnostic)
import CompletionItems


data KeliCommand 
    = Execute String 
    | Repl
    | Analyze String 
    | Suggest 
        String --filename
        Int    --line number
        Int    --column number
    deriving (Show)

allParser :: Parser KeliCommand
allParser = subparser (
    command "run" (info 
        (Execute 
            <$> (argument str (metavar "FILENAME")))
        (progDesc "Execute a Keli program (*.keli)"))
    <> 
    command "analyze" (info
        (Analyze 
            <$> (argument str (metavar "FILENAME")))
        (progDesc "Analyze a Keli program (*.keli) and display error as JSON."))
    <>
    command "suggest" (info
        (Suggest 
            <$> (argument str  (metavar "FILENAME"))
            <*> (argument auto (metavar "LINE_NUMBER(zero-based index)"))
            <*> (argument auto (metavar "COLUMN_NUMBER(zero-based index)")))
        (progDesc "Analyze a Keli program (*.keli) and suggest completion items."))
    <>
    command "repl" (info 
        (pure Repl)
        (progDesc "Starts the Keli REPL."))
    )

cli :: IO ()
cli = handleKeliCommand =<< execParser opts
  where
    opts = info (allParser <**> helper)
      ( fullDesc
     <> progDesc "Compile or interpret Keli program."
     <> header "The Keli Compiler" )

handleKeliCommand :: KeliCommand -> IO ()
handleKeliCommand input = 
    case input of 
        Execute filename -> do
            result <- keliInterpret filename 
            case result of 
                Right output ->
                    hPutStrLn stdout output
                
                Left err ->
                    hPutStrLn stderr err
        
        Repl -> 
            keliRepl
        
        Analyze filename -> do
            contents <- readFile filename
            (errors, _) <- keliCompile filename contents
            putStr (Char8.unpack (encode (concat (map toDiagnostic errors))))

        Suggest filename lineNumber columnNumber -> do
            completionItems <- suggestCompletionItemsAt filename (lineNumber, columnNumber)
            putStr (Char8.unpack (encode completionItems))



