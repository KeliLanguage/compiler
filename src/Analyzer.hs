{-# LANGUAGE MultiParamTypeClasses #-}
module Analyzer where

import Ast
import qualified Data.HashMap.Strict as H
import Data.List 
import Text.Parsec.Pos

buildDeclTable :: KeliDecl -> KeliDeclTable
buildDeclTable (Seq decls) = foldl
        ((\acc next -> 
            let key   = fst next in 
            let value = snd next in
            if H.member key acc 
                then case H.lookup key acc of Just x -> H.insert key (value:x) acc
                else H.insert key [value] acc
        )::KeliDeclTable -> (String, KeliDecl) -> KeliDeclTable)   -- reducer
        emptyKeliDeclTable -- initial value
        ((map toKeyValuePair idfulDecls) :: [(String, KeliDecl)]) -- foldee
    where 
        idfulDecls = filter (\x -> case x of 
                                        KeliConstDecl {constDeclId = id} -> case id of
                                            Just x -> True
                                            Nothing -> False
                                        KeliFuncDecl {} -> True) decls

        toKeyValuePair :: KeliDecl -> (String, KeliDecl)
        toKeyValuePair x =         
            let key = (case x of 
                        KeliConstDecl {constDeclId = id}  -> case id of 
                            Just x  -> (snd x)
                            Nothing -> undefined
                        KeliFuncDecl  {funcDeclIds = ids} -> (intercalate [] (map snd ids)))
            in (key, x)

emptyKeliDeclTable :: KeliDeclTable
emptyKeliDeclTable = H.empty

analzyeAst :: KeliDecl -> KeliDeclTable -> Either KeliError (KeliDecl, KeliDeclTable)
analzyeAst decls declTable = undefined

type KeliDeclTable = H.HashMap String [KeliDecl]

data KeliError = KeliError