module Ast.PreRaw where

import Text.Parsec.Pos

type StringToken = (SourcePos, String)
type NumberToken = (SourcePos, (Either Integer Double))

data Expr 
    = NumberExpr 
        NumberToken 

    | StringExpr 
        StringToken

    | Id     
        StringToken

    | FuncCall
        Expr -- subject
        [FuncCallTail]

    | Array 
        [Expr] -- elements
        SourcePos -- position of the `[` symbol

    deriving (Show,Eq)

data FuncCallTail 
    = MonoFuncCallTail StringToken
    | BiFuncCallTail StringToken Expr
    | PolyFuncCallTail [(StringToken, Expr)]
    deriving(Show, Eq)