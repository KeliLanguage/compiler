module Compiler where

import Data.List

import StaticError
import Control.Monad
import Analyzer
import Parser
import qualified Ast.Raw as Raw
import Env
import System.FilePath.Posix
import Data.List.Utils (replace)
import System.Directory
import Module
import qualified Data.HashMap.Strict as HashMap
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)


type ImportTree = [ImportNode]

data ImportNode 
    = ImportNode 
        String   -- importer's absolute filepath
        String   -- importees' absolute filepaths
    deriving (Eq, Show)

findImportersOf :: ImportTree -> String -> [String]
tree `findImportersOf` targetName =
    case tree of 
        [] ->
            []

        (ImportNode importerName importeeName):tree' ->
            let directImportersNames = 
                    let result = tree' `findImportersOf` targetName in
                    if importeeName == targetName then
                        importerName:result
                    else 
                        result

            -- we also need to find the importers of importers
            -- for detecting transitive dependencies
            in directImportersNames ++ concatMap (\name -> tree `findImportersOf` name) directImportersNames


-- used for representing a module with parse error
nullModule :: String -> Module 
nullModule name = Module name "" [] emptyEnv  []

type ModuleCache = HashMap.HashMap String Module

keliCompile 
    :: String 
    -> String 
    -> ModuleCache 
    -> ImportTree
    -> IO ([KeliError], Module, ModuleCache, ImportTree)
keliCompile filepath contents cache importTree = do  
    let currentModulename = takeBaseName (replace "\\" "/" filepath) -- Refer http://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath-Posix.html#v:takeBaseName
    importerFilePath <- makeAbsolute filepath
    case keliParse filepath contents of
        Right rawDecls -> do
            let (importStatements, nonImportRawDecls) = 
                    partition 
                        (\x -> case x of ImportDecl{} -> True; NonImportDecl{} -> False)
                        (map differentiateRawDecl rawDecls)

            let importersOfCurrentModule = importTree `findImportersOf` importerFilePath

            -- importeeErrors means errors at the imported files
            (importeeErrors, importedModules, updatedCache, updatedImportTree) <- 
                    foldM 
                        (\(prevErrors, prevModules, prevCache, prevTree) (ImportDecl importFilePath) -> do
                            let importPath = 
                                    if isAbsolute (snd importFilePath) then
                                        snd importFilePath
                                    else
                                        takeDirectory (replace "\\" "/" importerFilePath) ++ "/" ++ snd importFilePath
                            importeeFilePath <- canonicalizePath importPath
                            yes <- doesFileExist importeeFilePath
                            if yes then
                                -- check for circular imports
                                if importeeFilePath `elem` importersOfCurrentModule then
                                    let newError = KErrorCircularImport importFilePath (importerFilePath:importersOfCurrentModule) in
                                    return (newError:prevErrors, prevModules, prevCache, prevTree)
                                else
                                    -- update import tree
                                    let newImportTree = (ImportNode importerFilePath importeeFilePath):importTree in

                                    -- check if the module had been imported before or not
                                    case HashMap.lookup importeeFilePath prevCache of
                                        -- if already imported previously
                                        Just m ->
                                            return (prevErrors, prevModules ++ [m], prevCache, newImportTree)

                                        -- if never imported before
                                        Nothing -> do
                                            importedContents <- readFile importeeFilePath
                                            (currentErrors, currentModule, newCache, newImportTree') <- keliCompile importeeFilePath importedContents prevCache newImportTree
                                            return (
                                                prevErrors ++ currentErrors, 
                                                prevModules ++ [currentModule], 
                                                HashMap.insert importeeFilePath currentModule newCache,
                                                nub (newImportTree ++ newImportTree')) -- nub is for removing duplicates
                            else 
                                return (prevErrors ++ [KErrorCannotImport importFilePath], prevModules, prevCache, prevTree))
                        (([],[], cache, importTree) :: ([KeliError], [Module], ModuleCache, ImportTree))
                        importStatements

            let (currentErrors, currentEnv, currentDecls) = 
                    -- import intial environment here
                    let importedEnvs = map (\m -> (moduleFilepath m, moduleEnv m)) importedModules in
                    analyze (("<builtin>",initialEnv):importedEnvs) (map (\(NonImportDecl d) -> d) nonImportRawDecls)

            return (
                importeeErrors ++ currentErrors, 
                Module currentModulename importerFilePath importedModules currentEnv currentDecls,
                updatedCache,
                updatedImportTree)
        
        Left errs ->
            return (errs, nullModule currentModulename, cache, importTree)

data DifferentiationResult 
    = ImportDecl Raw.StringToken
    | NonImportDecl Raw.Decl
    deriving (Show)

differentiateRawDecl :: Raw.Decl -> DifferentiationResult
differentiateRawDecl 
    (Raw.IdlessDecl _ (Raw.FuncCall ((Raw.Id (_,"module"):(Raw.StringExpr filePath):[])) ((_,"import"):[])))  = 
    ImportDecl filePath

differentiateRawDecl other = NonImportDecl other