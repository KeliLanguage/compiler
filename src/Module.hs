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
    }


data Context 
    = Context {
        contextNextInt      :: Int,
        contextEnv          :: Env,
        contextImportedEnvs :: [(ModuleName,Env)]
    }

emptyContext :: Context
emptyContext = Context 0 emptyEnv [] 