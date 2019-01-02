{-# LANGUAGE MultiParamTypeClasses #-}
module Analyzer where

import Ast
import Data.HashMap.Strict hiding (map, filter)
import Data.List

buildDeclTable :: KeliDecl -> KeliDeclTable
buildDeclTable (Seq decls) = fromList (map toKeyValuePair idFullDecls)
    where 
        idFullDecls = filter (\x -> case x of 
                                        KeliConstDecl {constDeclId = id} -> case id of
                                            Just x -> True
                                            Nothing -> False
                                        KeliFuncDecl {} -> True) decls

        toKeyValuePair :: KeliDecl -> (String, KeliDecl)
        toKeyValuePair x =         
            let key = (case x of 
                        KeliConstDecl {constDeclId = id}  -> case id of 
                            Just x  -> x 
                            Nothing -> undefined
                        KeliFuncDecl  {funcDeclIds = ids} -> (intercalate [] ids))
            in (key, x)


analzyeAst :: KeliDecl -> KeliDeclTable -> Either KeliError (KeliDecl, KeliDeclTable)
analzyeAst decls declTable = undefined

type KeliDeclTable = HashMap String KeliDecl

data KeliError = KeliError