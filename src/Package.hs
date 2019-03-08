{-# LANGUAGE DeriveGeneric #-}
module Package where

import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as Char8
import System.Directory
import System.Info
import System.Process
import Data.Aeson.Encode.Pretty
import Data.Aeson

type Version = String

data Purse = Purse {
    _os           :: String,
    _arch         :: String,
    _compiler     :: Version,
    _git          :: Version,
    _node         :: Version,
    _dependencies :: [Dependency]
} deriving (Show, Generic, Eq, Read)

instance ToJSON Purse where

data Dependency = Dependency {
    _url :: String,
    _tag :: String
} deriving (Show, Generic, Eq, Read)

instance ToJSON Dependency where

createNewPackage :: String -> IO()
createNewPackage packageName = do
    putStrLn ("Creating package `" ++ packageName ++ "`")
    createDirectory packageName

    putStrLn ("Creating _src folder")
    createDirectory (packageName ++ "/_src")

    putStrLn ("Initializing purse.json")
    purse <- getPurse
    writeFile (packageName ++ "/_src/purse.json") (Char8.unpack (encodePretty purse))

    putStrLn ("Creating _test folder")
    createDirectory (packageName ++ "/_test")

    putStrLn ("Creating README file")
    writeFile (packageName ++ "/README.md") ("# " ++ packageName)

    putStrLn ("Creating LICENSE file")
    writeFile (packageName ++ "/LICENSE") ""

    putStrLn ("Creating .gitignore file")
    writeFile 
        (packageName ++ "/.gitignore") 
        (   "# ignore all folders\n" 
        ++  "/*\n\n"
        ++  "# except\n"
        ++  "!.gitignore\n"
        ++  "!README.md\n"
        ++  "!LICENSE\n"
        ++  "!_src/\n"
        ++  "!_test/\n")

    -- putStrLn ("Initializing as Git repository")

getPurse :: IO Purse
getPurse = do
    compilerVersion' <- readProcess "keli" ["version"] []   
    gitVersion       <- readProcess "git"  ["--version"] [] 
    nodeVersion      <- readProcess "node" ["--version"] []

    return Purse {
        _os           = os,
        _arch         = arch,
        _compiler     = init compilerVersion', -- init is used for removing the last newline character
        _node         = init nodeVersion,
        _git          = init gitVersion,
        _dependencies = []

    }

