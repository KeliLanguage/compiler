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
    funcDeclGenericParams :: [TypeParam],
    funcDeclParams        :: [FuncDeclParam],
    funcDeclIds           :: [StringToken],
    funcDeclReturnType    :: Type,
    funcDeclBody          :: Expr
} deriving (Show)

data TypeAlias =  TypeAlias StringToken Type deriving (Show)

data Type
    = TypeVariable 
        StringToken -- name
        (Maybe TypeConstraint) -- associated type constraint
        Bool -- is rigid or not? For more info, refer: https://mail.haskell.org/pipermail/haskell-cafe/2008-June/044622.html

    | ConcreteType
        Type'
    deriving (Show)

data Type'
    = TypeFloat
    | TypeInt
    | TypeString
    | TypeRecord 
        [(StringToken, Type)] -- key-type pairs
        -- TODO: implement generic record 
        -- (Maybe TypeParams) -- type params


    | TypeTaggedUnion TaggedUnion

    | TypeUndefined
    | TypeCarryfulTagConstructor 
        StringToken  -- tag
        Type         -- carry type
        TaggedUnion  -- belongingType
        (Maybe [Type]) -- type params

    | TypeRecordConstructor 
        [(StringToken, Type)] -- expected key-type pairs

    | TypeTagConstructorPrefix 
        StringToken -- name
        [Tag]       -- available tags
        (Maybe [Type]) -- type params

    | TypeTypeParam StringToken (Maybe TypeConstraint)
    | TypeType -- type of type

    | TypeIdentifiedCarryfulBranch
        Type -- carry type

    | TypeSelf -- for defining recursive type

    | TypeTypeConstructor TaggedUnion


data TaggedUnion = 
    TaggedUnion
        StringToken    -- name (name is compulsory, meaning that user cannot create anonymous tagged union)
        [StringToken]  -- ids
        [Tag]          -- list of tags
        (Maybe [Type])   -- type params
    deriving (Show)
    

instance Show Type' where
    show TypeFloat                                           = "*float"
    show (TypeIdentifiedCarryfulBranch t)                    = show t ++ " branch"
    show TypeInt                                             = "*Int"
    show TypeString                                          = "*String"
    show (TypeRecord kvs)                                    = "*record:" ++ show kvs
    show (TypeUndefined)                                     = "undefined"
    show (TypeCarryfulTagConstructor name _ _ _)             = "*carryful tag constructor:" ++ show name
    show (TypeRecordConstructor kvs)                         = "*record constructorshow:" ++ show kvs
    show (TypeTypeParam name _)                              = "*type param:" ++ show name
    show TypeType                                            = "*type type"
    show TypeSelf                                            = "*self"
    show TypeTypeConstructor{}                               = "*type constructor"
    show (TypeTaggedUnion (TaggedUnion name _ _ (Just typeParams))) = "*taggedunion{"++snd name++","++concat (map show typeParams) ++"}"
    show (TypeTaggedUnion (TaggedUnion name _ _ Nothing)) = "*tagged union{"++snd name++"}"


data TypeConstraint
    = ConstraintAny
    deriving (Show, Eq)

data TypeParam 
    = TypeParam 
        StringToken -- name
        (Maybe TypeConstraint)  -- associated type constriant
    deriving (Show)

data UnlinkedTag
    = UnlinkedCarrylessTag 
        StringToken -- tag

    | UnlinkedCarryfulTag
        StringToken -- tag
        Type        -- expected carry type

    deriving (Show)

data Tag
    = CarrylessTag 
        StringToken -- tag
        TaggedUnion -- belonging type

    | CarryfulTag
        StringToken -- tag
        Type        -- expected carry type
        TaggedUnion -- beloging type
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
        StringToken -- where is it defined?
        StringToken -- where is it used?


    | CarryfulTagConstructor 
        StringToken -- tag name
        Type        -- carry type
    
    | CarryfulTagExpr
        StringToken -- tag name
        Expr        -- carry expr

    | RecordConstructor [(StringToken, Type)]

    | TagConstructorPrefix

    | TypeConstructorPrefix

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
stringifyType (ConcreteType t) = stringifyType' t
stringifyType (TypeVariable {}) = ""

stringifyType' :: Type' -> String
stringifyType' t = case t of
        TypeFloat  -> "float"
        TypeInt    -> "Int"
        TypeString -> "String"
        TypeRecord kvs ->  error (show kvs)
        TypeTypeParam _ _ -> ""
        TypeType -> "type"
        TypeTaggedUnion (TaggedUnion name _ _ _) -> snd name
        _ -> error (show t)

-- For constraint type, we just return an empty string
instance Stringifiable TypeConstraint where
    toString c = case c of
        ConstraintAny -> "any"
        _ -> undefined
    
