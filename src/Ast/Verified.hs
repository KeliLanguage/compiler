module Ast.Verified where 

import Prelude hiding (id)
import Text.Parsec.Pos
import Data.List
import Data.Char
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

type StringToken = (SourcePos, String)

data Decl 
    = ConstDecl Const
    | FuncDecl Func
    | IdlessDecl Expr
    | TypeAliasDecl [StringToken] Type
    deriving (Show, Eq)

data Const = Const { 
    constDeclId    :: StringToken, -- because we can ignore the identifier
    constDeclValue :: Expr
} deriving (Show, Eq)

type FuncDeclParam = (StringToken, Type)
type FuncDeclConstraint = (StringToken, Type)

data Func = Func {
    funcDeclGenericParams :: [FuncDeclConstraint],
    funcDeclParams        :: [FuncDeclParam],
    funcDeclIds           :: [StringToken],
    funcDeclReturnType    :: Type,
    funcDeclBody          :: Expr
} deriving (Show, Eq)


data Type  
    = TypeFloat
    | TypeInt
    | TypeString
    | TypeRecord [(StringToken, Type)]
    | TypeTagUnion [Tag] -- list of tags
    | TypeAlias [StringToken] Type
    | TypeSingleton StringToken
    | TypeUndefined
    | TypeCarryfulTagConstructor 
        StringToken  -- tag
        Type     -- carry type
        Type     -- belonging type

    | TypeRecordConstructor [(StringToken, Type)]
    | TypeConstraint Constraint 
    | TypeParam StringToken Constraint
    | TypeType -- type of type
    | TypeCompound 
        StringToken -- name
        [Type] -- type params
    deriving (Show, Eq)

data Constraint
    = ConstraintAny
    deriving (Show, Eq)


data Tag
    = CarrylessTag 
        StringToken -- tag
        Type    -- belonging type

    | CarryfulTag
        StringToken -- tag
        Type    -- carry type
        Type    -- beloging type
            deriving (Show, Eq)

data Expr = 
    Expr 
        Expr' 
        Type  -- type of this expr
    deriving (Show, Eq)

data Expr'
    = IntExpr   (SourcePos, Integer) 
    | DoubleExpr (SourcePos, Double)
    | StringExpr StringToken
    | Id     StringToken
    | FuncCall {
        funcCallParams :: [Expr],
        funcCallIds    :: [StringToken],
        funcCallRef    :: Func
    }
    | Lambda {
        lambdaParams :: [StringToken],
        lambdaBody   :: Expr
    }
    | Record {
        recordKeyValues             :: [(StringToken, Expr)]
    }
    | RecordGetter {
        recordGetterSubject      :: Expr,
        recordGetterPropertyName :: StringToken
    }
    | RecordSetter {
        recordSetterSubject      :: Expr,
        recordSetterPropertyName :: StringToken,
        recordSetterNewValue     :: Expr
    }
    | TagMatcher {
        tagMatcherSubject    :: Expr,
        tagMatcherBranches   :: [(StringToken, Expr)], -- [(Tag, Expr)]
        tagMatcherElseBranch :: Maybe Expr
    } 
    | CarrylessTagConstructor 
        StringToken -- tag name

    | CarryfulTagConstructor 
        StringToken -- tag name
        Type        -- carry type
    
    | CarryfulTagExpr
        StringToken -- tag name
        Expr        -- carry expr

    | RecordConstructor [(StringToken, Type)]

    deriving (Show,Eq)


class Identifiable a where
    getIdentifier :: a -> StringToken

instance Identifiable Decl where
    getIdentifier d = case d of
        ConstDecl c -> getIdentifier c
        FuncDecl  f -> getIdentifier f

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
instance Identifiable Func where
    getIdentifier (Func{funcDeclIds=ids, funcDeclParams=params})
        = (
            fst (head ids)
            ,
            intercalate "$" (map (toValidJavaScriptId . snd) ids) ++ "$$" ++ intercalate "$" (map (stringifyType . snd) params) 
        )

-- Basically, this function will convert all symbols to its corresponding ASCII code
-- e.g. toValidJavaScriptId "$" = "_36"
toValidJavaScriptId :: String -> String
toValidJavaScriptId s = "_" ++ concat (map (\x -> if (not . isAlphaNum) x then show (ord x) else [x]) s)


instance Identifiable Const where
    getIdentifier c = constDeclId c



instance Identifiable Type where
    getIdentifier x = (newPos "" (-1) (-1), stringifyType x)


-- What is toString for?
-- It is for generating the identifier for each particular functions
-- For example, the following function:
--     this:str.reverse = undefined
-- Will have an id of something like _reverse_str
-- So that the function lookup process can be fast (during analyzing function call)

class Stringifiable a where
    toString :: a -> String

stringifyType :: Type -> String
stringifyType t = case t of
        TypeFloat  -> "float"
        TypeInt    -> "int"
        TypeString -> "str"
        TypeRecord kvs -> error (show kvs)
        TypeTagUnion tags -> undefined 
        TypeAlias ids _ -> concat (map snd ids)
        TypeConstraint c -> toString c
        TypeParam _ _ -> ""
        TypeType -> "type"
        _ -> error (show t)

-- For constraint type, we just return an empty string
instance Stringifiable Constraint where
    toString c = case c of
        ConstraintAny -> "any"
        _ -> undefined


typeEquals :: Type -> Type -> Bool
x `typeEquals` y = unpackType x == unpackType y
    
unpackType :: Type -> Type
unpackType t =
    case t of
        TypeAlias _ type' -> type'
        _ -> t
