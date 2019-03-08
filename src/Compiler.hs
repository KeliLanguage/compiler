module Compiler where

import Data.List

import StaticError
import Control.Monad
import Analyzer
import Parser
import qualified Ast.Raw as Raw
import Env
import System.FilePath.Posix
import System.Directory
import Module
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

-- used for representing a module with parse error
nullModule :: String -> Module 
nullModule name = Module name [] emptyEnv  []

keliCompile :: String -> String -> IO ([KeliError], Module)
keliCompile filepath contents = do  
    let currentModulename = takeBaseName filepath -- Refer http://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath-Posix.html#v:takeBaseName
    case keliParse filepath contents of
        Right rawDecls -> do
            let (importStatements, nonImportRawDecls) = 
                    partition 
                        (\x -> case x of ImportDecl{} -> True; NonImportDecl{} -> False)
                        (map differentiateRawDecl rawDecls)

            -- importeeErrors means errors at the imported files
            (importeeErrors, importedModules) <- 
                    foldM 
                        (\(prevErrors, prevModules) (ImportDecl importFilePath) -> do
                            absoluteFilePath <- makeAbsolute filepath
                            let importPath = 
                                    if isAbsolute (snd importFilePath) then
                                        snd importFilePath
                                    else
                                        takeDirectory absoluteFilePath ++ "/" ++ snd importFilePath
                            canonicalizedImportPath <- canonicalizePath importPath
                            yes <- doesFileExist canonicalizedImportPath
                            if yes then do
                                importedContents <- readFile canonicalizedImportPath
                                (currentErrors, currentModule) <- keliCompile canonicalizedImportPath importedContents

                                -- filter out idless decls, as specified at https://keli-language.gitbook.io/doc/specification/section-5-declarations#5-0-evaluated-declarations
                                -- let idfulDecls = filter (\d -> case d of V.IdlessDecl{} -> False; _ -> True ) decls'

                                return (prevErrors ++ currentErrors, prevModules ++ [currentModule])
                            else 
                                return (prevErrors ++ [KErrorCannotImport importFilePath], prevModules))
                        (([],[]) :: ([KeliError],[Module]))
                        importStatements

            let (currentErrors, currentEnv, currentDecls) = 
                    -- import intial environment here
                    let importedEnvs = map (\m -> (moduleName m, moduleEnv m)) importedModules in
                    analyze (("<builtin>",initialEnv):importedEnvs) (map (\(NonImportDecl d) -> d) nonImportRawDecls)

            return (importeeErrors ++ currentErrors, Module currentModulename importedModules currentEnv currentDecls)
        
        Left errs ->
            return (errs, nullModule currentModulename)

data DifferentiationResult 
    = ImportDecl Raw.StringToken
    | NonImportDecl Raw.Decl
    deriving (Show)

differentiateRawDecl :: Raw.Decl -> DifferentiationResult
differentiateRawDecl 
    (Raw.IdlessDecl _ (Raw.FuncCall ((Raw.Id (_,"module"):(Raw.StringExpr filePath):[])) ((_,"import"):[])))  = 
    ImportDecl filePath

differentiateRawDecl other = NonImportDecl other