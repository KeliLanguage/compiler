module Ast.Verified where 

import Prelude hiding (id)
import Text.Parsec.Pos
import Data.List
import Data.Char
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

type StringToken = (SourcePos, String)
type NumberToken = (SourcePos, (Either Integer Double))

data VerifiedDecl 
    = VerifiedConstDecl VerifiedConst
    | VerifiedFuncDecl VerifiedFunc
    | VerifiedIdlessDecl VerifiedExpr
    | VerifiedTypeAliasDecl [StringToken] VerifiedType
    deriving (Show, Eq)

data VerifiedConst = VerifiedConst { 
    constDeclId    :: StringToken, -- because we can ignore the identifier
    constDeclValue :: VerifiedExpr
} deriving (Show, Eq)

type VerifiedFuncDeclParam = (StringToken, VerifiedType)
type VerifiedFuncDeclConstraint = (StringToken, VerifiedType)

data VerifiedFunc = VerifiedFunc {
    funcDeclGenericParams :: [VerifiedFuncDeclConstraint],
    funcDeclParams        :: [VerifiedFuncDeclParam],
    funcDeclIds           :: [StringToken],
    funcDeclReturnType    :: VerifiedType,
    funcDeclBody          :: VerifiedExpr
} deriving (Show, Eq)


data VerifiedType  
    = VerifiedTypeFloat
    | VerifiedTypeInt
    | VerifiedTypeString
    | VerifiedTypeRecord [(StringToken, VerifiedType)]
    | VerifiedTypeTagUnion [VerifiedTag] -- list of tags
    | VerifiedTypeAlias [StringToken] VerifiedType
    | VerifiedTypeSingleton StringToken
    | VerifiedTypeUndefined
    | VerifiedTypeCarryfulTagConstructor 
        StringToken  -- tag
        VerifiedType     -- carry type
        VerifiedType     -- belonging type

    | VerifiedTypeRecordConstructor [(StringToken, VerifiedType)]
    | VerifiedTypeConstraint VerifiedConstraint 
    | VerifiedTypeParam StringToken VerifiedConstraint
    | VerifiedTypeType -- type of type
    | VerifiedTypeCompound 
        StringToken -- name
        [VerifiedType] -- type params
    deriving (Show, Eq)

data VerifiedConstraint
    = VerifiedConstraintAny
    deriving (Show, Eq)


data VerifiedTag
    = VerifiedTagCarryless 
        StringToken -- tag
        VerifiedType    -- belonging type

    | VerifiedTagCarryful
        StringToken -- tag
        VerifiedType    -- carry type
        VerifiedType    -- beloging type
            deriving (Show, Eq)

data VerifiedExpr 
    = VerifiedNumber NumberToken 
    | VerifiedString StringToken
    | VerifiedId     StringToken
    | VerifiedFuncCall {
        funcCallParams :: [VerifiedExpr],
        funcCallIds    :: [StringToken],
        funcCallRef    :: Maybe VerifiedFunc
    }
    | VerifiedLambda {
        lambdaParams :: [StringToken],
        lambdaBody   :: VerifiedExpr
    }
    | VerifiedRecord {
        recordKeyValues             :: [(StringToken, VerifiedExpr)],
        recordExpectedPropTypePairs :: Maybe [(StringToken, VerifiedType)] 
            -- if Just, means it is created using record constructor
            -- if Nothing, means this is an anonymous record
    }
    | VerifiedRecordGetter {
        recordGetterSubject      :: VerifiedExpr,
        recordGetterPropertyName :: StringToken
    }
    | VerifiedRecordSetter {
        recordSetterSubject              :: VerifiedExpr,
        recordSetterPropertyName         :: StringToken,
        recordSetterNewValue             :: VerifiedExpr,
        recordSetterExpectedPropertyType :: VerifiedType,
        recordSetterReturnType           :: VerifiedType
    }
    | VerifiedTagMatcher {
        tagMatcherSubject    :: VerifiedExpr,
        tagMatcherBranches   :: [(StringToken, VerifiedExpr)], -- [(Tag, VerifiedExpr)]
        tagMatcherElseBranch :: Maybe VerifiedExpr
    } 
    | VerifiedTagConstructor {
        tagConstructorId    :: StringToken,
        tagConstructorCarry :: Maybe VerifiedExpr
    } 
    | VerifiedVerifiedExpr {
        _expr :: VerifiedExpr,
        _type :: VerifiedType
    }
    | VerifiedRecordConstructor [(StringToken, VerifiedType)]

    deriving (Show,Eq)


class Identifiable a where
    getIdentifier :: a -> StringToken

instance Identifiable VerifiedDecl where
    getIdentifier d = case d of
        VerifiedConstDecl c -> getIdentifier c
        VerifiedFuncDecl  f -> getIdentifier f

-- Each function identifier shall follows the following format:
--
--      <front part>$$<back part>
--      id1$id2$id3$$paramType1$paramType2$paramType3
--
--  where <front part> is function names and <back part> is param types
-- 
-- Example:
--      this:str.replace old:str with new:str | str = undefined
-- Shall have id of
--      replace$old$$str$str$str 
--
-- This format is necessary, so that when we do function lookup,
--  we can still construct back the function details from its id when needed
--  especially when looking up generic functions
instance Identifiable VerifiedFunc where
    getIdentifier (VerifiedFunc{funcDeclIds=ids, funcDeclParams=params})
        = (
            fst (head ids)
            ,
            intercalate "$" (map (toValidJavaScriptId . snd) ids) ++ "$$" ++ intercalate "$" (map (stringifyType . snd) params) 
        )

-- Basically, this function will convert all symbols to its corresponding ASCII code
-- e.g. toValidJavaScriptId "$" = "_36"
toValidJavaScriptId :: String -> String
toValidJavaScriptId s = "_" ++ concat (map (\x -> if (not . isAlphaNum) x then show (ord x) else [x]) s)


instance Identifiable VerifiedConst where
    getIdentifier c = constDeclId c



instance Identifiable VerifiedType where
    getIdentifier x = (newPos "" (-1) (-1), stringifyType x)


-- What is toString for?
-- It is for generating the identifier for each particular functions
-- For example, the following function:
--     this:str.reverse = undefined
-- Will have an id of something like _reverse_str
-- So that the function lookup process can be fast (during analyzing function call)

class Stringifiable a where
    toString :: a -> String

stringifyType :: VerifiedType -> String
stringifyType t = case t of
        VerifiedTypeFloat  -> "float"
        VerifiedTypeInt    -> "int"
        VerifiedTypeString -> "str"
        VerifiedTypeRecord kvs -> error (show kvs)
        VerifiedTypeTagUnion tags -> undefined 
        VerifiedTypeAlias ids _ -> concat (map snd ids)
        VerifiedTypeConstraint c -> toString c
        VerifiedTypeParam _ _ -> ""
        VerifiedTypeType -> "type"
        _ -> error (show t)

-- For constraint type, we just return an empty string
instance Stringifiable VerifiedConstraint where
    toString c = case c of
        VerifiedConstraintAny -> "any"
        _ -> undefined


typeEquals :: VerifiedType -> VerifiedType -> Bool
x `typeEquals` y = unpackType x == unpackType y
    
unpackType :: VerifiedType -> VerifiedType
unpackType t =
    case t of
        VerifiedTypeAlias _ type' -> type'
        _ -> t
