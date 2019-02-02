module Ast.Verified where 

import Prelude hiding (id)
import Text.Parsec.Pos
import Data.List
import Data.Char
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

type StringToken = (SourcePos, String)

nullStringToken :: StringToken
nullStringToken = (newPos "" (-1) (-1), "null")

newStringToken :: String -> StringToken
newStringToken value = (newPos "" (-1) (-1), value)

newStringToken' :: (Int,Int,String) -> StringToken
newStringToken' (line,col,value) = (newPos "" line col, value)
data Decl 
    = ConstDecl Const
    | FuncDecl Func
    | IdlessDecl Expr
    deriving (Show)

data Const = Const { 
    constDeclId    :: StringToken, -- because we can ignore the identifier
    constDeclValue :: Expr
} deriving (Show)

type FuncDeclParam = (StringToken, Type)
type FuncDeclConstraint = (StringToken, TypeParam)

data Func = Func {
    funcDeclGenericParams :: [FuncDeclConstraint],
    funcDeclParams        :: [FuncDeclParam],
    funcDeclIds           :: [StringToken],
    funcDeclReturnType    :: Type,
    funcDeclBody          :: Expr
} deriving (Show)

data TypeAlias =  TypeAlias [StringToken] Type deriving (Show)



data Type
    = TypeFloat
    | TypeInt
    | TypeString
    | TypeRecord [(StringToken, Type)]
    | TypeTagUnion 
        StringToken --name (name is compulsory, meaning that user cannot create anonymous tagged union)
        [Tag]       -- list of tags

    | TypeUndefined
    | TypeCarryfulTagConstructor 
        StringToken  -- tag
        Type     -- carry type
        Type     -- belonging type

    | TypeRecordConstructor [(StringToken, Type)]
    | TypeTagConstructorPrefix StringToken [Tag]
    | TypeTypeParam StringToken (Maybe TypeConstraint)
    | TypeType -- type of type
    | TypeCompound 
        StringToken -- name
        [Type] -- type params

    | TypeIdentifiedCarryfulBranch
        Type -- carry type

    | TypeSelf -- for defining recursive type

instance Show Type where
    show TypeFloat                              = "float"
    show (TypeIdentifiedCarryfulBranch t)       = show t ++ " branch"
    show TypeInt                                = "Int"
    show TypeString                             = "String"
    show (TypeRecord kvs)                       = "record:" ++ show kvs
    show (TypeTagUnion name _)                  = show name
    show (TypeUndefined)                        = "undefined"
    show (TypeCarryfulTagConstructor name _ _)  = show name
    show (TypeRecordConstructor kvs)            = show kvs
    show (TypeTypeParam name _)                     = show name
    show TypeType                               = "type"
    show (TypeCompound name params)             = show name ++ show params
    show TypeSelf                               = "$self"


instance Eq Type where
    TypeFloat                           == TypeFloat                        = True
    TypeInt                             == TypeInt                          = True
    TypeString                          == TypeString                       = True
    TypeTagUnion name1 _                == TypeTagUnion name2 _             = name1 == name2
    TypeCarryfulTagConstructor x _ _    == TypeCarryfulTagConstructor y _ _ = x == y
    TypeRecordConstructor kvs1          == TypeRecordConstructor kvs2       = kvs1 == kvs2
    TypeType                            == TypeType                         = True
    TypeCompound name1 params1          == TypeCompound name2 params2       = name1 == name2 && params1 == params2
    TypeTypeParam name1 _               == TypeTypeParam name2 _            = name1 == name2
    TypeUndefined                       == _                                = error "Cannot compare type of undefined"
    _                                   == TypeUndefined                    = error "Cannot compare type of undefined"

    -- record type is handled differently, because we want to have structural typing
    -- NOTE: kts means "key-type pairs"
    TypeRecord kts1 == TypeRecord kts2 = 
        let removeSourcePos kts = map (\((_,key),t) -> (key, t)) kts in
        let sortedKvs1 = sortOn fst (removeSourcePos kts1) in
        let sortedKvs2 = sortOn fst (removeSourcePos kts2) in
        sortedKvs1 == sortedKvs2

    -- anything other pattern should be false
    _ == _ = False

data TypeConstraint
    = ConstraintAny
    deriving (Show, Eq)

data TypeParam 
    = TypeParam 
        (Maybe TypeConstraint)  -- associated type constriant
    deriving (Show)

data Tag
    = CarrylessTag 
        StringToken -- tag
        Type    -- belonging type

    | CarryfulTag
        StringToken -- tag
        Type    -- carry type
        Type    -- beloging type
            deriving (Show)

tagnameOf :: Tag -> StringToken
tagnameOf (CarrylessTag t _) = t
tagnameOf (CarryfulTag t _ _) = t

instance Eq Tag where
    (CarrylessTag t1 _) == (CarrylessTag t2 _) = t1 == t2
    (CarryfulTag t1 _ _) == (CarryfulTag t2 _ _) = t1 == t2
    _ == _ = False

data Expr = 
    Expr 
        Expr' 
        Type  -- type of this expr
    deriving (Show)

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

    | TagConstructorPrefix

    | RetrieveCarryExpr Expr

    | FFIJavascript StringToken

    deriving (Show)


class Identifiable a where
    getIdentifier :: a -> (String, [StringToken])

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
--      this:String.replace old:String with new:String | String = undefined
-- Shall have id of
--      replace$with$$String$String$String 
--
-- This format is necessary, so that when we do function lookup,
--  we can still construct back the function details from its id when needed
--  especially when looking up generic functions
instance Identifiable Func where
    getIdentifier (Func{funcDeclIds=ids, funcDeclParams=params})
        = ( 
            intercalate "$" (map (toValidJavaScriptId . snd) ids) ++ "$$" ++ intercalate "$" (map (stringifyType . snd) params)
            ,
            ids
         )

-- Basically, this function will convert all symbols to its corresponding ASCII code
-- e.g. toValidJavaScriptId "$" = "_36"
toValidJavaScriptId :: String -> String
toValidJavaScriptId s = "_" ++ concat (map (\x -> if (not . isAlphaNum) x then show (ord x) else [x]) s)


instance Identifiable Const where
    getIdentifier c = let x = constDeclId c in (snd x, [x])


-- What is toString for?
-- It is for generating the identifier for each particular functions
-- For example, the following function:
--     this:String.reverse = undefined
-- Will have an id of something like _reverse_str
-- So that the function lookup process can be fast (during analyzing function call)

class Stringifiable a where
    toString :: a -> String

stringifyType :: Type -> String
stringifyType t = case t of
        TypeFloat  -> "float"
        TypeInt    -> "Int"
        TypeString -> "String"
        TypeRecord kvs ->  error (show kvs)
        TypeTagUnion name _ -> snd name 
        TypeTypeParam _ _ -> ""
        TypeType -> "type"
        _ -> error (show t)

-- For constraint type, we just return an empty string
instance Stringifiable TypeConstraint where
    toString c = case c of
        ConstraintAny -> "any"
        _ -> undefined


typeEquals :: Type -> Type -> Bool
x `typeEquals` y = x == y
    
