module Ast.Verified where 

import Prelude hiding (id)
import Text.Parsec.Pos
import Data.List
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import qualified Ast.Raw as Raw

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

data Func = Func {
    funcDeclGenericParams :: [Type], -- all should be BoundedTypeVar
    funcDeclParams        :: [(StringToken, TypeAnnotation)],
    funcDeclIds           :: [StringToken],
    funcDeclReturnType    :: Type,
    funcDeclBody          :: Expr
} deriving (Show)

data TypeAnnotation 
    = TypeAnnotSimple 
        StringToken -- name
        Type -- ref
    
    | TypeAnnotCompound
        StringToken -- constructor name
        [(StringToken, TypeAnnotation)] -- key-type pairs
        Type -- ref
    deriving (Show)

getTypeRef :: TypeAnnotation -> Type
getTypeRef x = case x of
    TypeAnnotSimple _ t -> t
    TypeAnnotCompound _ _ t -> t

data Type
    = TypeFloat
    | TypeInt
    | TypeString
    | TypeRecord 
        (Maybe StringToken)   -- associated name
        [(StringToken, Type)] -- prop-type pairs
        -- TODO: implement generic record 
        -- (Maybe TypeParams) -- type params


    | TypeTaggedUnion TaggedUnion

    | TypeUndefined
    | TypeCarryfulTagConstructor 
        StringToken           -- tagname
        [(StringToken, Type)] -- expected prop-type pairs
        TaggedUnion           -- belongingType

    | TypeRecordConstructor 
        (Maybe StringToken)   -- record type alias name
        [(StringToken, Type)] -- expected key-type pairs

    | TypeTagConstructorPrefix 
        StringToken -- name
        [Tag]       -- available tags
        [Type]      -- type params

    | TypeTypeParam StringToken (Maybe TypeConstraint)
    | TypeType -- type of type

    | TypeSelf -- for defining recursive type

    | TypeTypeConstructor TaggedUnion

    | FreeTypeVar String (Maybe TypeConstraint)

    | BoundedTypeVar StringToken (Maybe TypeConstraint)


data TaggedUnion = 
    TaggedUnion
        StringToken      -- name (name is compulsory, meaning that user cannot create anonymous tagged union)
        [StringToken]    -- ids
        [Tag]            -- list of tags
        [Type] -- type params

instance Show TaggedUnion where
    show (TaggedUnion (_,name) ids _ _) = 
        name ++ "." ++ intercalate "$" (map snd ids)

instance Show Type where
    show TypeFloat                                           = "*float"
    show TypeInt                                             = "*Int"
    show TypeString                                          = "*String"
    show (TypeRecord name kvs)                               = "*record: (show name)"
    show (TypeUndefined)                                     = "undefined"
    show (TypeCarryfulTagConstructor name _ _)               = "*carryful tag constructor:" ++ show name
    show (TypeRecordConstructor _ kvs)                       = undefined -- "*record constructorshow:" ++ show kvs
    show (TypeTypeParam name _)                              = "*type param:" ++ show name
    show TypeType                                            = "*type type"
    show TypeSelf                                            = "*self"
    show TypeTypeConstructor{}                               = "*type constructor"
    show (TypeTaggedUnion (TaggedUnion name _ _ typeParams)) = "*taggedunion{"++snd name++","++concat (map show typeParams) ++"}"
    show (FreeTypeVar name _) = "*freetypevar:" ++ name
    show (BoundedTypeVar name _) = "*boundedtypevar:" ++ snd name


data TypeConstraint
    = ConstraintAny
    deriving (Show, Eq)

data UnlinkedTag
    = UnlinkedCarrylessTag 
        StringToken -- tag

    | UnlinkedCarryfulTag
        StringToken -- tag
        [(StringToken, TypeAnnotation)] -- key-type pairs

    deriving (Show)

data Tag
    = CarrylessTag 
        StringToken -- tag
        TaggedUnion -- belonging type

    | CarryfulTag
        StringToken             -- tag
        [(StringToken, Type)]   -- expected key-type pairs
        TaggedUnion             -- beloging type
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
    | FuncApp {
        funcAppFunc :: Expr,
        funcAppArg  :: Expr
    }
    | PartiallyInferredLambda 
        StringToken -- param
        Raw.Expr    -- body

    
    | Lambda 
        (StringToken, Type) -- param
        Expr        -- body
    
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
        tagMatcherBranches   :: [TagBranch],
        tagMatcherElseBranch :: Maybe Expr
    } 
    | CarrylessTagExpr 
        StringToken -- where is it defined?
        StringToken -- where is it used?


    | CarryfulTagConstructor 
        StringToken             -- tag name
        [(StringToken, Type)]   -- expected prop-type pairs
    
    | CarryfulTagExpr
        StringToken -- tag name
        [(StringToken, Expr)] -- key-value pairs

    | RecordConstructor 
        (Maybe StringToken)   -- record type alias name
        [(StringToken, Type)]

    | TagConstructorPrefix StringToken

    | TypeConstructorPrefix StringToken

    | FFIJavascript StringToken

    deriving (Show)

data VerifiedTagname = VerifiedTagname StringToken
    deriving (Show)

data TagBranch 
    = CarrylessTagBranch 
        VerifiedTagname -- tag name
        Expr 
    
    | CarryfulTagBranch
        VerifiedTagname -- tag name
        [(StringToken, StringToken, Type)] -- property binding as in [(from, to, type)]
        Expr 

    | ElseBranch
        Expr
    deriving (Show)

stringifyType :: Type -> String
stringifyType t = case t of
        TypeFloat  -> "Float"
        TypeInt    -> "Int"
        TypeString -> "String"
        TypeRecord name propTypePairs ->  
            case name of 
                Just n ->
                    snd n
                Nothing ->
                    "record." ++ 
                    intercalate " " 
                        (map (\((_,prop),type') ->
                            prop ++ "(" ++ stringifyType type' ++  ")")
                            propTypePairs)

        TypeTypeParam _ _ -> ""
        TypeType -> "Type"
        TypeTaggedUnion (TaggedUnion name _ _ _) -> snd name
        BoundedTypeVar name _ -> snd name
        FreeTypeVar name _ -> name
        TypeUndefined -> "Undefined"
        _ -> error (show t)
