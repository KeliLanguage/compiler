module SymbolTable where

import StaticError
import Data.Map.Ordered (OMap, (|>), assocs, member, empty)
import Ast
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

data KeliSym
    = KeliSymFunc KeliFunc 
    | KeliSymConst KeliConst
    | KeliSymSingleton StringToken
    | KeliSymType KeliType
    | KeliSymTag KeliTag
    deriving(Show)

instance Identifiable KeliSym where
    getIdentifier sym = case sym of 
        (KeliSymFunc f)            -> getIdentifier f
        (KeliSymConst c)           -> getIdentifier c
        (KeliSymSingleton id)      -> id
        (KeliSymType t)            -> getIdentifier t
        (KeliSymTag t)             -> 
            case t of
            KeliTagCarryless x _   -> x
            KeliTagCarryful  x _ _ -> x
    

type KeliSymTab = OMap String KeliSym

buildSymTab :: [KeliDecl] -> Either KeliError KeliSymTab 
buildSymTab decls = foldl
        ((\acc next -> 
            case acc of 
                Left  e -> Left e
                Right table ->
                    let (token,value) = next in 
                    let key = snd token in
                    if member key table 
                        then Left (KErrorDuplicatedId token)
                        else Right (table |> (key ,toKeliSym value))
        )::Either KeliError KeliSymTab -> (StringToken, KeliDecl) -> Either KeliError KeliSymTab)   -- reducer
        (Right emptyKeliSymTab) -- initial value
        ((map toKeyValuePair idfulDecls) :: [(StringToken, KeliDecl)]) -- foldee
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
                            other   -> error("Unkown primitive type: " ++ other)
                    else 
                        KeliSymConst c

                _        -> KeliSymConst c

            KeliFuncDecl  f -> KeliSymFunc f

        idfulDecls = filter (\x -> case x of KeliIdlessDecl _ -> False; _ -> True) decls

        toKeyValuePair :: KeliDecl -> (StringToken, KeliDecl)
        toKeyValuePair x = (getIdentifier x, x)
        

emptyKeliSymTab :: KeliSymTab
emptyKeliSymTab = empty
