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


data CliInput 
    = Execute String 
    | Interactive Bool
    | Analyze String 
    | Suggest 
        String --filename
        Int    --line number
        Int    --column number
    deriving (Show)

parseExecute :: Parser CliInput
parseExecute = Execute
        <$> strOption
            (  long "execute"
            <> short 'e'
            <> metavar "FILENAME"
            <> help "Execute a Keli program (*.keli)" )

parseInteractive :: Parser CliInput
parseInteractive = Interactive
        <$> switch
            (  long "interactive"
            <> short 'i'
            <> help "Run Keli compiler in interactive mode (REPL)" )

parseAnalyze :: Parser CliInput
parseAnalyze = Analyze
        <$> strOption
            (  long "analyze"
            <> short 'a'
            <> metavar "FILENAME"
            <> help "Analyze a Keli program (*.keli) and display error as JSON." )

parseSuggest :: Parser CliInput
parseSuggest = Suggest
        <$> strOption
            (  long "suggest"
            <> short 's'
            <> metavar "FILENAME"
            <> help "Analyze a Keli program (*.keli) and display completion items." )
        <*> option auto
            (  long "line"
            <> short 'l'
            <> metavar "LINE"
            <> help "To be used with --suggest" )
        <*> option auto
            (  long "column"
            <> short 'c'
            <> metavar "COLUMN"
            <> help "To be used with --suggest" )



allParser :: Parser CliInput
allParser 
    =   parseExecute 
    <|> parseInteractive 
    <|> parseAnalyze 
    <|> parseSuggest

cli :: IO ()
cli = handleCliInput =<< execParser opts
  where
    opts = info (allParser <**> helper)
      ( fullDesc
     <> progDesc "Compile or interpret Keli program."
     <> header "The Keli Compiler" )

handleCliInput :: CliInput -> IO ()
handleCliInput input = 
    case input of 
        Execute filename -> do
            result <- keliInterpret filename 
            case result of 
                Right output ->
                    hPutStrLn stdout output
                
                Left err ->
                    hPutStrLn stderr err
        
        Interactive {} -> 
            keliRepl
        
        Analyze filename -> do
            contents <- readFile filename
            (errors, _) <- keliCompile filename contents
            putStr (Char8.unpack (encode (concat (map toDiagnostic errors))))

        Suggest filename lineNumber columnNumber -> do
            completionItems <- suggestCompletionItemsAt filename (lineNumber, columnNumber)
            putStr (Char8.unpack (encode completionItems))



