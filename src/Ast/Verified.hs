module Ast.Verified where 

import Prelude hiding (id)
import Text.Parsec.Pos
import Data.List
-- import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import qualified Ast.Raw as Raw

type StringToken = (SourcePos, String)

nullStringToken :: StringToken
nullStringToken = (newPos "" (-1) (-1), "null")

newStringToken :: String -> StringToken
newStringToken value = (newPos "" (-1) (-1), value)

newStringToken' :: (Int,Int,String) -> StringToken
newStringToken' (line,col,value) = (newPos "" line col, value)

data Scope 
    = FromCurrentScope
    | FromImports 
        String -- imporeted module name
    deriving (Show)


data Decl 
    = ConstDecl 
        StringToken
        Expr

    | FuncDecl 
        FuncSignature -- function signature
        Expr          -- function body

    | IdlessDecl 
        Expr

    | ObjectAliasDecl
        StringToken
        [(StringToken, TypeAnnotation)]

    | TaggedUnionDecl
        TaggedUnion
    deriving (Show)

-- Func means the signature of a function
data FuncSignature = FuncSignature {
    funcDeclDocString     :: Maybe String,
    funcDeclGenericParams :: [Type], -- all should be BoundedTypeVar
    funcDeclParams        :: [(StringToken, TypeAnnotation)],
    funcDeclIds           :: [StringToken],
    funcDeclReturnType    :: Type
} deriving (Show)


data TypeAnnotation 
    = TypeAnnotSimple 
        StringToken -- name
        Type -- ref
    
    | TypeAnnotCompound
        StringToken -- constructor name
        [(StringToken, TypeAnnotation)] -- key-type pairs
        Type -- ref

    | TypeAnnotObject
        [(StringToken, TypeAnnotation)] -- key-type pairs
    deriving (Show)

getTypeRef :: TypeAnnotation -> Type
getTypeRef x = case x of
    TypeAnnotSimple _ t -> t
    TypeAnnotCompound _ _ t -> t
    TypeAnnotObject propTypeAnnotPairs ->
        TypeObject Nothing (map (\(k, typeAnnot) -> (k, getTypeRef typeAnnot)) propTypeAnnotPairs)

data Type
    = TypeFloat
    | TypeInt
    | TypeString
    | TypeObject 
        (Maybe StringToken)   -- associated name
        [(StringToken, Type)] -- prop-type pairs
        -- TODO: implement generic object 
        -- (Maybe TypeParams) -- type params


    | TypeTaggedUnion TaggedUnion

    | TypeUndefined

    | TypeObjectConstructor 
        (Maybe StringToken)   -- object type alias name
        [(StringToken, Type)] -- expected key-type pairs

    | TypeTagConstructorPrefix 
        StringToken -- name
        [Tag]       -- available tags
        [Type]      -- type params
        Scope

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
    show (TaggedUnion (_,name) ids tags typeParams) = 
        "*taggedunion{"++name++","++concat (map show typeParams) ++ "," ++ show (length tags) ++ "}"

instance Show Type where
    show TypeFloat                                           = "*float"
    show TypeInt                                             = "*Int"
    show TypeString                                          = "*String"
    show (TypeObject name _)                                 = "*object:" ++ show name
    show (TypeUndefined)                                     = "undefined"
    show (TypeObjectConstructor _ _)                         = undefined -- "*object constructorshow:" ++ show kvs
    show (TypeTypeParam name _)                              = "*type param:" ++ show name
    show TypeType                                            = "*type type"
    show TypeSelf                                            = "*self"
    show TypeTypeConstructor{}                               = "*type constructor"
    show (TypeTaggedUnion t) = show t
    show (FreeTypeVar name _) = "*freetypevar:" ++ name
    show (BoundedTypeVar name _) = "*boundedtypevar:" ++ snd name
    show (TypeTagConstructorPrefix{}) = "TypeTagConstructorPrefix"


data TypeConstraint
    = ConstraintAny
    deriving (Show, Eq)

data UnlinkedTag
    = UnlinkedCarrylessTag 
        StringToken -- tag

    | UnlinkedCarryfulTag
        StringToken -- tag
        TypeAnnotation
    deriving (Show)

data Tag
    = CarrylessTag 
        StringToken -- tag
        TaggedUnion -- belonging type

    | CarryfulTag
        StringToken   -- tag
        Type          -- carry type
        TaggedUnion   -- beloging type
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
    | Array [Expr]
    | GlobalId -- for global constants
        StringToken -- actual usage
        StringToken -- reference (where is this id originally defined)
        Scope

    | LocalId  -- for function and lambda parameters
        StringToken -- actual usage
        StringToken -- reference (where is this id originally defined)

    | FuncCall {
        funcCallParams :: [Expr], -- for transpilation
        funcCallIds    :: [StringToken], -- for reporting error
        funcCallRef    :: (Scope, FuncSignature) -- for transpilation
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
    
    | Object {
        objectKeyValues             :: [(StringToken, Expr)]
    }
    | ObjectGetter {
        objectGetterSubject      :: Expr,
        objectGetterPropertyName :: StringToken
    }
    | ObjectSetter {
        objectSetterSubject      :: Expr,
        objectSetterPropertyName :: StringToken,
        objectSetterNewValue     :: Expr
    }
    | ObjectLambdaSetter 
        Expr        -- subject
        StringToken -- property name
        StringToken -- lambda param
        Expr        -- lambda body

    | TagMatcher {
        tagMatcherSubject    :: Expr,
        tagMatcherBranches   :: [TagBranch],
        tagMatcherElseBranch :: Maybe Expr
    } 
    | CarrylessTagExpr 
        StringToken -- where is it defined?
        StringToken -- where is it used?
        Scope 

    | CarryfulTagExpr
        StringToken -- tag name
        Expr        -- carry expr
        Scope

    | CarryfulTagConstructor 
        StringToken             -- tag name
        TypeAnnotation          -- expected carry type
    

    | ObjectConstructor 
        (Maybe StringToken)   -- object type alias name
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
        StringToken -- binding
        Expr 

    | ElseBranch
        Expr
    deriving (Show)

stringifyType :: Type -> String
stringifyType t = case t of
        TypeFloat  -> "Float"
        TypeInt    -> "Int"
        TypeString -> "String"
        TypeObject name propTypePairs ->  
            case name of 
                Just n ->
                    snd n
                Nothing ->
                    "object." ++ 
                    intercalate " " 
                        (map (\((_,prop),type') ->
                            prop ++ "(" ++ stringifyType type' ++  ")")
                            propTypePairs)

        TypeTypeParam _ _ -> ""
        TypeType -> "Type"
        TypeTaggedUnion (TaggedUnion name ids _ innerTypes) -> 
            let tailPart = intercalate " " (map (\((_,id), t') -> id ++ "(" ++ stringifyType t' ++ ")") (zip ids innerTypes)) in
            snd name ++ (if length ids > 0 then "." ++ tailPart else "")

        BoundedTypeVar name _ -> snd name
        FreeTypeVar name _ -> name
        TypeUndefined -> "Undefined"
        _ -> error (show t)
