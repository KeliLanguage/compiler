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
import Module
import System.FilePath
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
                            let importedFilePath =
                                    if isAbsolute (snd importFilePath) then
                                        snd importFilePath
                                    else
                                        takeDirectory absoluteFilePath ++ "/" ++ snd importFilePath
                            yes <- doesFileExist importedFilePath
                            if yes then do
                                importedContents <- readFile importedFilePath
                                (currentErrors, currentModule) <- keliCompile importedFilePath importedContents

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
    (Raw.IdlessDecl (Raw.FuncCall ((Raw.Id (_,"module"):(Raw.StringExpr filePath):[])) ((_,"import"):[])))  = 
    ImportDecl filePath

differentiateRawDecl other = NonImportDecl other