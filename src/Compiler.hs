module Compiler where

import Data.List

import StaticError
import Control.Monad
import Analyzer
import Parser
import qualified Ast.Raw as Raw
import Transpiler
import Env
import System.Directory
import qualified Ast.Verified as V
import System.FilePath
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

keliCompile :: String -> IO ([KeliError], [Env], [V.Decl])
keliCompile filepath = do  
    contents <- readFile filepath
    -- keliParse filepath contents
    -- >>= analyze 
    -- >>= \decls -> return (keliTranspile decls)

    case keliParse filepath contents of
        Right rawDecls -> do
            let (importStatements, nonImportRawDecls) = 
                    partition 
                        (\x -> case x of ImportDecl{} -> True; NonImportDecl{} -> False)
                        (map differentiateRawDecl rawDecls)

            -- importeeErrors means errors at the imported files
            (importeeErrors, importedEnvs, importedDecls) <- 
                    foldM 
                        (\(prevErrors, prevEnvs, prevDecls) (ImportDecl filePath) -> do
                            absoluteFilePath <- makeAbsolute filepath
                            let importedFilePath = takeDirectory absoluteFilePath ++ "/" ++ snd filePath
                            yes <- doesFileExist importedFilePath
                            if yes then do
                                (errors', envs', decls') <- keliCompile importedFilePath

                                -- filter out idless decls, as specified at https://keli-language.gitbook.io/doc/specification/section-5-declarations#5-0-evaluated-declarations
                                let idfulDecls = filter (\d -> case d of V.IdlessDecl{} -> False; _ -> True ) decls'

                                return (prevErrors ++ errors', prevEnvs ++ envs', prevDecls ++ idfulDecls)
                            else 
                                return (prevErrors ++ [KErrorCannotImport filePath], prevEnvs, prevDecls))
                        (([],[],[]) :: ([KeliError],[Env],[V.Decl]))
                        importStatements

            let (currentErrors, currentEnv, currentDecls) = 
                    -- import intial environment here
                    analyze (initialEnv:importedEnvs) (map (\(NonImportDecl d) -> d) nonImportRawDecls)

            return (importeeErrors ++ currentErrors, importedEnvs ++ [currentEnv], importedDecls ++ currentDecls) 
                
        
        Left errs ->
            return (errs, [], [])

data DifferentiationResult 
    = ImportDecl Raw.StringToken
    | NonImportDecl Raw.Decl
    deriving (Show)

differentiateRawDecl :: Raw.Decl -> DifferentiationResult
differentiateRawDecl 
    (Raw.IdlessDecl (Raw.FuncCall ((Raw.Id (_,"module"):(Raw.StringExpr filePath):[])) ((_,"import"):[])))  = 
    ImportDecl filePath

differentiateRawDecl other = NonImportDecl other