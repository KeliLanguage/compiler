module Cli where


import Options.Applicative
import Data.Semigroup ((<>))
import Data.Aeson

import Interpreter
import Repl
import Parser(keliParse)
import Analyzer(analyze)
import Diagnostics(toDiagnostic)




data CliInput 
    = Execute String 
    | Interactive Bool
    | Analyze String 
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

allParser :: Parser CliInput
allParser = parseExecute <|> parseInteractive <|> parseAnalyze 

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
                    print "[]"
                Left err ->
                    print (encode (toDiagnostic err))



