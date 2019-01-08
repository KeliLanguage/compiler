module Ast where 

import Text.Parsec.Pos
import Data.List

type StringToken = (SourcePos, String)
type NumberToken = (SourcePos, (Either Integer Double))

data KeliDecl 
    = KeliConstDecl KeliConst
    | KeliFuncDecl KeliFunc
    | KeliIdlessDecl KeliExpr
    deriving (Show)

data KeliConst = KeliConst { 
    constDeclId    :: StringToken, -- because we can ignore the identifier
    constDeclValue :: KeliExpr,
    constDeclType  :: Maybe KeliType
} deriving (Show)

data KeliFunc = KeliFunc {
    funcDeclGenericParams :: [KeliFuncDeclParam],
    funcDeclParams        :: [KeliFuncDeclParam],
    funcDeclIds           :: [StringToken],
    funcDeclReturnType    :: KeliType,
    funcDeclBody          :: KeliExpr
} deriving (Show)

data KeliFuncDeclParam 
    = KeliFuncDeclParam {
        funcDeclParamId   :: StringToken,
        funcDeclParamType :: KeliType
    }
    deriving (Show)

data KeliType  
    = KeliTypeUnchecked KeliExpr
    | KeliTypeFloat
    | KeliTypeInt
    | KeliTypeString
    | KeliTypeRecord [(StringToken, KeliType)]
    | KeliTypeTagUnion [KeliTag]
    | KeliTypeAlias StringToken KeliType
    deriving (Show, Eq)

data KeliTag = KeliTag StringToken (Maybe KeliType) deriving (Show, Eq)

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
    | KeliTypeCheckedExpr {
        _expr :: KeliExpr,
        _type :: KeliType
    }
    | KeliTypeExpr KeliType
    deriving (Show, Eq)

class Identifiable a where
    getIdentifier :: a -> String

instance Identifiable KeliDecl where
    getIdentifier d = case d of
        KeliConstDecl c -> getIdentifier c
        KeliFuncDecl  f -> getIdentifier f

instance Identifiable KeliFunc where
    getIdentifier (KeliFunc{funcDeclIds=ids, funcDeclParams=params})
        = intercalate "$" (map snd ids) ++ intercalate "$" (map (toString . funcDeclParamType) params) 

instance Identifiable KeliConst where
    getIdentifier c = snd (constDeclId c)

instance Identifiable KeliType where
    getIdentifier = toString

class Stringifiable a where
    toString :: a -> String

instance Stringifiable KeliType where
    toString t = case t of
        KeliTypeFloat  -> "float"
        KeliTypeInt    -> "int"
        KeliTypeString -> "str"
        KeliTypeRecord kvs -> undefined
        KeliTypeTagUnion tags -> undefined
        KeliTypeAlias (_,id) _ -> id
        KeliTypeUnchecked expr -> "unknown"


class HaveType a where
    getType :: a -> KeliType

instance HaveType KeliExpr where
    getType (KeliTypeCheckedExpr _ exprType) = exprType
    getType e = KeliTypeUnchecked e