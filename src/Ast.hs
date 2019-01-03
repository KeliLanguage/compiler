module Ast where 

import Text.Parsec.Pos

type StringToken = (SourcePos, String)
type NumberToken = (SourcePos, (Either Integer Double))

data KeliDecl 
    = KeliConstDecl { 
        constDeclId    :: Maybe StringToken, -- because we can ignore the identifier
        constDeclValue :: KeliExpr,
        constDeclType  :: Maybe KeliExpr
    }
    | KeliFuncDecl {
        funcDeclGenericParams :: [KeliFuncDeclParam],
        funcDeclParams        :: [KeliFuncDeclParam],
        funcDeclIds           :: [StringToken],
        funcDeclReturnType    :: KeliExpr,
        funcDeclBody          :: KeliExpr
    }
    deriving (Show)

data KeliFuncDeclParam 
    = KeliFuncDeclParam {
        funcDeclParamId   :: StringToken,
        funcDeclParamType :: KeliExpr
    }
    deriving (Show)

data KeliExpr 
    = KeliNumber NumberToken 
    | KeliString StringToken
    | KeliId     StringToken
    | KeliFuncCall {
        funcCallParams :: [KeliExpr],
        funcCallIds    :: [StringToken]
    }
    | KeliLambda {
        lambdaParams :: [StringToken],
        lambdaBody   :: KeliExpr
    }
    | KeliTypeCheckedExpr {
        _expr :: KeliExpr,
        _type :: KeliExpr
    }
    deriving (Show)
