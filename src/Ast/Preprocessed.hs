module Ast.Preprocessed where 

import Prelude hiding (id)
import Text.Parsec.Pos
import Data.List
import Data.Char
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

type StringToken = (SourcePos, String)
type NumberToken = (SourcePos, (Either Integer Double))

data Decl 
    = ConstDecl Const
    | FuncDecl Func
    | IdlessDecl Expr
    deriving (Show, Eq)

data Const = Const { 
    constDeclId    :: StringToken, 
    constDeclValue :: Expr
} deriving (Show, Eq)

type FuncDeclParam = (StringToken, Expr)
type FuncDeclConstraint = (StringToken, Expr)

data Func = Func {
    funcDeclGenericParams :: [FuncDeclConstraint],
    funcDeclParams        :: [FuncDeclParam],
    funcDeclIds           :: [StringToken],
    funcDeclReturnType    :: Expr,
    funcDeclBody          :: Expr
} deriving (Show, Eq)


data Tag
    = TagCarryless 
        StringToken -- tag
        Expr    -- belonging type

    | TagCarryful
        StringToken -- tag
        Expr    -- carry type
        Expr    -- beloging type
            deriving (Show, Eq)

data Expr 
    = NumberExpr NumberToken 
    | StringExpr StringToken
    | Id     StringToken
    | FuncCall {
        funcCallParams :: [Expr],
        funcCallIds    :: [StringToken],
        funcCallRef    :: Maybe Func
    }
    | Lambda {
        lambdaParams :: [StringToken],
        lambdaBody   :: Expr
    }
    | Record {
        recordKeyValues             :: [(StringToken, Expr)],
        recordExpectedPropTypePairs :: Maybe [(StringToken, Expr)] 
            -- if Just, means it is created using record constructor
            -- if Nothing, means this is an anonymous record
    }
    deriving (Show,Eq)
