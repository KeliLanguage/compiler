module Module where

import qualified Ast.Verified as V
import Env

type ModuleName = String

data Module 
    = Module {
        moduleName     :: ModuleName,
        moduleImported :: [Module],
        moduleEnv      :: Env,
        moduleDecls    :: [V.Decl]
    } deriving (Show)


data Context 
    = Context {
        contextNextInt      :: Int,
        contextEnv          :: Env,
        contextImportedEnvs :: [(ModuleName,Env)]
    } deriving (Show)

emptyContext :: Context
emptyContext = Context 0 emptyEnv [] 