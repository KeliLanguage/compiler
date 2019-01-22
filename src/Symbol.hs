module Symbol where

import Text.Parsec.Pos
import Data.Map.Ordered 

import qualified Ast.Raw as Raw

data KeliSymbol
    = KeliSymFunc [Raw.Func]
    | KeliSymConst Raw.Const
    | KeliSymSingleton Raw.StringToken
    | KeliSymType Raw.Type
    | KeliSymTag Raw.Tag
    | KeliSymInlineExprs [Raw.Expr] -- for storing expr from Raw.IdlessConst
    deriving(Show)

instance Raw.Identifiable KeliSymbol where
    getIdentifier sym = case sym of 
        (KeliSymFunc f)            -> error "Shouldn't reach here"
        (KeliSymConst c)           -> Raw.getIdentifier c
        (KeliSymSingleton id)      -> id
        (KeliSymType t)            -> Raw.getIdentifier t
        (KeliSymTag t)             -> 
            case t of
            Raw.TagCarryless x _   -> x
            Raw.TagCarryful  x _ _ -> x
        (KeliSymInlineExprs _)     -> (newPos "" 0  0, "@inline_exprs")
    

type KeliSymTab = OMap String KeliSymbol

emptyKeliSymTab :: KeliSymTab
emptyKeliSymTab = empty


class HaveType a where
    getType :: a -> Raw.Type

instance HaveType Raw.Expr where
    getType (Raw.TypeCheckedExpr _ exprType) = exprType
    getType e = Raw.TypeUnverified e

