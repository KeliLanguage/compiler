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
import qualified Data.HashMap.Strict as HashMap
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

-- used for representing a module with parse error
nullModule :: String -> Module 
nullModule name = Module name "" [] emptyEnv  []

type ModuleCache = HashMap.HashMap String Module

keliCompile :: String -> String -> ModuleCache -> IO ( [KeliError], Module, ModuleCache)
keliCompile filepath contents cache = do  
    let currentModulename = takeBaseName filepath -- Refer http://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath-Posix.html#v:takeBaseName
    absoluteFilePath <- makeAbsolute filepath
    case keliParse filepath contents of
        Right rawDecls -> do
            let (importStatements, nonImportRawDecls) = 
                    partition 
                        (\x -> case x of ImportDecl{} -> True; NonImportDecl{} -> False)
                        (map differentiateRawDecl rawDecls)

            -- importeeErrors means errors at the imported files
            (importeeErrors, importedModules, updatedCache) <- 
                    foldM 
                        (\(prevErrors, prevModules, prevCache) (ImportDecl importFilePath) -> do
                            let importPath = 
                                    if isAbsolute (snd importFilePath) then
                                        snd importFilePath
                                    else
                                        takeDirectory absoluteFilePath ++ "/" ++ snd importFilePath
                            canonicalizedImportPath <- canonicalizePath importPath
                            yes <- doesFileExist canonicalizedImportPath
                            if yes then 
                                -- check if the module had been imported before or not
                                case HashMap.lookup canonicalizedImportPath prevCache of
                                    -- if already imported previously
                                    Just m ->
                                        return (prevErrors, prevModules ++ [m], prevCache)

                                    -- if never imported before
                                    Nothing -> do
                                        importedContents <- readFile canonicalizedImportPath
                                        (currentErrors, currentModule, nextCache) <- keliCompile canonicalizedImportPath importedContents prevCache
                                        return (
                                            prevErrors ++ currentErrors, 
                                            prevModules ++ [currentModule], 
                                            HashMap.insert canonicalizedImportPath currentModule nextCache )
                            else 
                                return (prevErrors ++ [KErrorCannotImport importFilePath], prevModules, prevCache))
                        (([],[], cache) :: ([KeliError],[Module],ModuleCache))
                        importStatements

            let (currentErrors, currentEnv, currentDecls) = 
                    -- import intial environment here
                    let importedEnvs = map (\m -> (moduleName m, moduleEnv m)) importedModules in
                    analyze (("<builtin>",initialEnv):importedEnvs) (map (\(NonImportDecl d) -> d) nonImportRawDecls)

            return (
                importeeErrors ++ currentErrors, 
                Module currentModulename absoluteFilePath importedModules currentEnv currentDecls,
                updatedCache)
        
        Left errs ->
            return (errs, nullModule currentModulename, cache)

data DifferentiationResult 
    = ImportDecl Raw.StringToken
    | NonImportDecl Raw.Decl
    deriving (Show)

differentiateRawDecl :: Raw.Decl -> DifferentiationResult
differentiateRawDecl 
    (Raw.IdlessDecl _ (Raw.FuncCall ((Raw.Id (_,"module"):(Raw.StringExpr filePath):[])) ((_,"import"):[])))  = 
    ImportDecl filePath

differentiateRawDecl other = NonImportDecl other