module Symbol where

import Text.Parsec.Pos

import Ast

data KeliSymbol
    = KeliSymFunc [KeliFunc]
    | KeliSymConst KeliConst
    | KeliSymSingleton StringToken
    | KeliSymType KeliType
    | KeliSymTag KeliTag
    | KeliSymInlineExprs [KeliExpr] -- for storing expr from KeliIdlessConst
    deriving(Show)

instance Identifiable KeliSymbol where
    getIdentifier sym = case sym of 
        (KeliSymFunc f)            -> error "Shouldn't reach here"
        (KeliSymConst c)           -> getIdentifier c
        (KeliSymSingleton id)      -> id
        (KeliSymType t)            -> getIdentifier t
        (KeliSymTag t)             -> 
            case t of
            KeliTagCarryless x _   -> x
            KeliTagCarryful  x _ _ -> x
        (KeliSymInlineExprs _)     -> (newPos "" 0  0, "@inline_exprs")
    
