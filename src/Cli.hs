module Cli where


import Options.Applicative
import Data.Semigroup ((<>))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8

import Interpreter
import Repl
import Parser(keliParse)
import Analyzer(analyze, analyzeDecls)
import Diagnostics(toDiagnostic)
import Symbol(emptyKeliSymTab)
import CompletionItems




data CliInput 
    = Execute String 
    | Interactive Bool
    | Analyze String 
    | Suggest String
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
            contents <- readFile filename
            output <- keliInterpret filename contents
            print output
        
        Interactive {} -> 
            keliRepl
        
        Analyze filename -> do
            contents <- readFile filename
            case keliParse filename contents >>= analyze of
                Right _ ->
                    putStr "[]"
                Left errs ->
                    putStr (Char8.unpack (encode (concat (map toDiagnostic errs))))

        Suggest filename -> do
            contents <- readFile filename
            case keliParse filename contents of
                Right ast ->
                    let (_,_,symbols) = analyzeDecls emptyKeliSymTab ast in
                    let completionItems = map toCompletionItem symbols in
                    putStr (Char8.unpack (encode (concat completionItems)))

                Left errs ->
                    putStr "[]"
 

        




