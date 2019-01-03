{-# LANGUAGE MultiParamTypeClasses #-}
module Analyzer where

import Ast
import qualified Data.HashMap.Strict as H
import Data.List 
import Text.Parsec.Pos

buildDeclTable :: [KeliDecl] -> Either KeliError KeliDeclTable 
buildDeclTable decls = foldl
        ((\acc next -> 
            case acc of 
                Left  e -> Left e
                Right table ->
                    let key   = fst next in 
                    let value = snd next in
                    if H.member key table 
                        then Left KeliError
                        else Right (H.insert key value table)
        )::Either KeliError KeliDeclTable -> (String, KeliDecl) -> Either KeliError KeliDeclTable)   -- reducer
        (Right emptyKeliDeclTable) -- initial value
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
                        KeliFuncDecl  {
                            funcDeclIds = ids,
                            funcDeclParams = params
                        } -> (intercalate [] (map snd ids)))
            in (key, x)

emptyKeliDeclTable :: KeliDeclTable
emptyKeliDeclTable = H.empty

analzyeAst :: [KeliDecl] -> KeliDeclTable -> ([KeliDecl], [KeliError])
analzyeAst decls declTable = foldl 
        (\acc next -> 
            let (prevDecls, errors1) = acc in 
            let (decl2, errors2)     = analyzeDecl next declTable in
            (decl2:prevDecls, errors1 ++ errors2))
        ([], [])
        decls

analyzeDecl :: KeliDecl -> KeliDeclTable -> (KeliDecl, [KeliError])
analyzeDecl decl table = case decl of
    constDecl@(
        KeliConstDecl {
            constDeclId=id,
            constDeclValue=expr,
            constDeclType=expectedType
        }) -> let (checkedExpr, errors) = typeCheckExpr expr table in
            ((KeliConstDecl id checkedExpr expectedType), errors)
    KeliFuncDecl  {}  -> undefined

typeCheckExpr :: KeliExpr -> KeliDeclTable -> (KeliExpr, [KeliError])
typeCheckExpr expr table = undefined
    

type KeliDeclTable = H.HashMap String KeliDecl

data KeliError = KeliError