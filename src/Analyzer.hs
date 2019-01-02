{-# LANGUAGE MultiParamTypeClasses #-}
module Analyzer where

import Ast
import Data.HashMap.Strict hiding (map)
import Data.List

buildDeclTable :: KeliDecl -> KeliDeclTable
buildDeclTable (Seq decls) = fromList (
    map (\x -> (
        let key = (case x of KeliConstDecl {constDeclId = id}  -> case id of 
                                                                    Just x  -> x 
                                                                    Nothing -> undefined
                             KeliFuncDecl  {funcDeclIds = ids} -> (intercalate [] ids))
        in (key, x)
    )) decls)

analzyeAst :: KeliDecl -> KeliDeclTable -> Either KeliError (KeliDecl, KeliDeclTable)
analzyeAst = undefined

type KeliDeclTable = HashMap String KeliDecl

data KeliError = KeliError