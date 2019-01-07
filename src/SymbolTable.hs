module SymbolTable where

import qualified Data.HashMap.Strict as H
import StaticError

import Ast
import Data.List

data KeliSym
    = KeliSymFunc KeliFunc 
    | KeliSymConst KeliConst
    | KeliSymSingleton StringToken
    | KeliSymType KeliType

type KeliSymTab = H.HashMap String KeliSym

buildSymTab :: [KeliDecl] -> Either KeliError KeliSymTab 
buildSymTab decls = foldl
        ((\acc next -> 
            case acc of 
                Left  e -> Left e
                Right table ->
                    let key   = fst next in 
                    let value = snd next in
                    if H.member key table 
                        then Left (KErrorDuplicatedId)
                        else Right (H.insert key (toKeliSym value) table)
        )::Either KeliError KeliSymTab -> (String, KeliDecl) -> Either KeliError KeliSymTab)   -- reducer
        (Right emptyKeliSymTab) -- initial value
        ((map toKeyValuePair idfulDecls) :: [(String, KeliDecl)]) -- foldee
    where 
        toKeliSym decl = case decl of
            KeliConstDecl (c@KeliConst{constDeclId=(_,id)}) -> case constDeclValue c of
                KeliId s@(_,id') -> 
                    if id == id' then 
                        KeliSymSingleton s 
                    else if id' == "_primitive" then
                        case id of
                            "int"   -> KeliSymType KeliTypeInt
                            "str"   -> KeliSymType KeliTypeString
                            "float" -> KeliSymType KeliTypeFloat
                            other -> error("Unkown primitive type: " ++ other)
                    else 
                        KeliSymConst c
                _        -> KeliSymConst c
            KeliFuncDecl  f -> KeliSymFunc f

        idfulDecls = filter (\x -> case x of KeliIdlessDecl _ -> False; _ -> True) decls

        toKeyValuePair :: KeliDecl -> (String, KeliDecl)
        toKeyValuePair x = (getIdentifier x, x)
        

emptyKeliSymTab :: KeliSymTab
emptyKeliSymTab = H.empty