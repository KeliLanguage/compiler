module Cli where

import Text.Pretty.Simple (pPrint)
import StaticError
import Interpreter
import Repl

import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))



data CliInput 
    = Execute { 
        execute     :: String
    }
    | Interactive {
        interactive :: Bool
    }
    deriving (Show)

executeParser :: Parser CliInput
executeParser = Execute
        <$> strOption
            (  long "execute"
            <> short 'e'
            <> metavar "FILENAME"
            <> help "Execute a Keli program (*.keli)" )

interactiveParser :: Parser CliInput
interactiveParser = Interactive
        <$> switch
            (  long "interactive"
            <> short 'i'
            <> help "Run Keli compiler in interactive mode (REPL)" )


cli :: IO ()
cli = handleCliInput =<< execParser opts
  where
    opts = info ((executeParser <|> interactiveParser) <**> helper)
      ( fullDesc
     <> progDesc "Compile or interpret Keli program."
     <> header "The Keli Compiler" )

handleCliInput :: CliInput -> IO ()
handleCliInput (Execute filename) = do
        contents <- readFile filename
        output <- keliInterpret contents
        print output

handleCliInput (Interactive _ ) = keliRepl

handleCliInput _ = return ()

