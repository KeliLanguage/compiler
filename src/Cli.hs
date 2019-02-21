{-# LANGUAGE BangPatterns #-}
module Cli where


import Options.Applicative
import Data.Semigroup ((<>))
import Data.Aeson
import Data.List
import Data.Foldable
import Data.Sequence
import qualified Data.ByteString.Lazy.Char8 as Char8
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

import Interpreter
import Repl
import Parser(keliParse)
import Analyzer(analyze, analyzeDecls)
import Diagnostics(toDiagnostic)
import Env(emptyEnv)
import CompletionItems
import StaticError




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
            contents <- readFile filename
            result <- keliInterpret filename contents
            case result of 
                Right output ->
                    putStrLn output
                
                Left err ->
                    print (err)
        
        Interactive {} -> 
            keliRepl
        
        Analyze filename -> do
            contents <- readFile filename
            case keliParse filename contents >>= analyze of
                Right _ ->
                    putStr "[]"
                Left errs ->
                    putStr (Char8.unpack (encode (concat (map toDiagnostic errs))))

        Suggest filename lineNumber columnNumber -> do
            contents <- readFile filename
            let lines' = lines contents
            let currentChar = lines' !! lineNumber !! columnNumber
            let contents' = 
                    -- if the current character is a dot(.)
                        -- replace it with semicolon(;)
                        -- so that we can parse KeliIncompleteFuncCall properly
                    if currentChar == '.' then
                        let lines'' = fromList (map fromList lines') in
                        let result = (
                                update 
                                    -- at
                                    lineNumber 

                                    -- with new value
                                    (update 
                                        -- at
                                        columnNumber

                                        -- with new value
                                        ';'

                                        -- over
                                        (lines'' `index` lineNumber))

                                    -- over
                                    lines'') in

                        intercalate "\n" (map toList (toList (result)))
                    else
                        contents

            case keliParse filename contents' of
                Right decls ->
                    let completionItems = suggestCompletionItems decls in
                    putStr (Char8.unpack (encode completionItems))

                Left errs ->
                    putStr "[]"
 

        




