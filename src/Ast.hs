module Ast where 


data KeliDecl 
    = Seq [KeliDecl] -- Sequences of Declarations
    | KeliConstDecl { 
        constDeclId    :: String,
        constDeclValue :: KeliExpr,
        constDeclType  :: Maybe KeliExpr
    }
    | KeliFuncDecl {
        funcDeclParams     :: [KeliFuncDeclParam],
        funcDeclIds        :: [String],
        funcDeclReturnType :: KeliExpr,
        funcDeclBody       :: KeliExpr
    }
    deriving (Show)

data KeliFuncDeclParam 
    = KeliFuncDeclParam {
        funcDeclParamId   :: String,
        funcDeclParamType :: KeliExpr
    }
    deriving (Show)

data KeliExpr 
    = KeliNumber (Either Integer Double)
    | KeliString String
    | KeliArray  [KeliExpr]
    | KeliTuple  [KeliExpr]
    | KeliId     String
    | KeliFuncCall {
        funcCallParams :: [KeliExpr],
        funcCallIds    :: [String]
    }
    | KeliLambda {
        lambdaParams :: [String],
        lambdaBody   :: KeliExpr
    }
    deriving (Show)
