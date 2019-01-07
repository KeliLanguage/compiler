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

data KeliType  
    = KeliTypeFloat
    | KeliTypeInt
    | KeliTypeString
    | KeliTypeRecord [(StringToken, KeliType)]
    | KeliTypeTagUnion [KeliTag]
    deriving (Show)

data KeliTag = KeliTag StringToken (Maybe KeliType) deriving (Show)

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
        _type :: KeliType
    }
    | KeliRecord {
        recordKeyValues :: [(StringToken, KeliExpr)]
    }
    | KeliRecordGetter {
        recordGetterSubject      :: KeliExpr,
        recordGetterPropertyName :: String
    }
    | KeliRecordSetter {
        recordSetterSubject      :: KeliExpr,
        recordSetterPropertyName :: StringToken,
        recordSetterNewValue     :: KeliExpr
    }
    | KeliTagChecker {
        tagCheckerSubject  :: KeliExpr,
        tagCheckerBranches :: [(StringToken, KeliExpr)] -- [(Tag, KeliExpr)]
    } 
    | KeliTagConstructor {
        tagConstructorId    :: StringToken,
        tagConstructorCarry :: Maybe KeliExpr
    } 
    deriving (Show)
