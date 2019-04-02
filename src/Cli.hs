{-# LANGUAGE BangPatterns #-}
module Cli where


import Options.Applicative
import Data.Semigroup ((<>))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.HashMap.Strict as HashMap
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

import System.IO
import Interpreter
import Repl
import Transpiler
import Package
import Compiler
import Diagnostics(toDiagnostic)
import CompletionItems

keliCompilerVersion :: String
keliCompilerVersion = "0.0.2-alpha"

data KeliCommand 
    = Execute 
        String  -- filename
        Bool    -- whether to show line number or not
    | Repl
    | Analyze 
        String -- filename
    | Compile 
        String -- filename
    | Suggest 
        String --filename
        Int    --line number
        Int    --column number
    | NewPackage 
        String -- package name
    | AddDependency
        String -- git repo url
        String -- tag
    | Version
    | Install 
        String -- path to purse.json
    deriving (Show)

allParser :: Parser KeliCommand
allParser = subparser (
    command "run" (info 
        (Execute 
            <$> (argument str (metavar "FILENAME"))
            <*> switch
                    ( long "show-line-number"
                    <> short 'l'
                    <> help "Where to show line number or not." ))
        (progDesc "Execute a Keli program (*.keli)"))
    <> 
    command "analyze" (info
        (Analyze 
            <$> (argument str (metavar "FILENAME")))
        (progDesc "Analyze a Keli program (*.keli) and display error as JSON."))
    <>
    command "compile" (info
        (Compile 
            <$> (argument str (metavar "FILENAME")))
        (progDesc "Compile a Keli program (*.keli) into JavaScript file."))

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
    <>
    command "new-package" (info 
        (NewPackage
            <$> (argument str (metavar "FILENAME")))
        (progDesc "Create a new Keli package"))
    <>
    command "add-dependency" (info 
        (AddDependency
            <$> (argument str (metavar "GIT_REPO_URL"))
            <*> (argument str (metavar "TAG")))
        (progDesc "Create a new Keli package"))
    <>
    command "install" (info 
        (Install
            <$> (argument str (metavar "PATH_TO_PURSE.JSON")))
        (progDesc "Install dependencies based on the specified purse.json"))
    <>
    command "version" (info 
        (pure Version)
        (progDesc "Get the version of this Keli compiler."))
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
        Execute filename showLineNumber -> do
            result <- keliInterpret showLineNumber filename 
            case result of 
                Right output ->
                    hPutStrLn stdout output
                
                Left err ->
                    hPutStrLn stderr err

        Compile filename -> do
            contents <- readFile filename
            (errors, module', _, _) <- keliCompile filename contents (HashMap.empty) []
            if length errors > 0 then
                putStr (Char8.unpack (encode (concat (map toDiagnostic errors))))
            else
                putStr (transpileModule False module')
        
        Repl -> 
            keliRepl
        
        Analyze filename -> do
            contents <- readFile filename
            (errors, _, _, _) <- keliCompile filename contents (HashMap.empty) []
            putStr (Char8.unpack (encode (concat (map toDiagnostic errors))))

        Suggest filename lineNumber columnNumber -> do
            completionItems <- suggestCompletionItemsAt filename (lineNumber, columnNumber)
            putStr (Char8.unpack (encode completionItems))

        Install pursePath -> 
            installDeps pursePath

        Version ->
            putStrLn keliCompilerVersion

        NewPackage packageName -> do
            createNewPackage packageName

        AddDependency gitRepoUrl tag -> do
            addDependency gitRepoUrl tag




