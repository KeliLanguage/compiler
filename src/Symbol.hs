module Symbol where

import Text.Parsec.Pos
import Data.Map.Ordered 

import qualified Ast.Raw as Raw
import qualified Ast.Verified as Verified

data KeliSymbol
    = KeliSymFunc [Verified.Func]
    | KeliSymConst Raw.StringToken Verified.Expr
    | KeliSymTag Verified.Tag
    | KeliSymType Verified.Type
    | KeliSymInlineExprs [Verified.Expr] -- for storing expr from Raw.IdlessConst
    deriving(Show)

instance Verified.Identifiable KeliSymbol where
    getIdentifier sym = case sym of 
        (KeliSymFunc f)            -> error "Shouldn't reach here"
        (KeliSymType t)            -> Verified.getIdentifier t
        (KeliSymTag t)             -> 
            case t of
                Verified.CarrylessTag x _   -> x
                Verified.CarryfulTag  x _ _ -> x
        (KeliSymInlineExprs _)     -> (newPos "" 0  0, "@inline_exprs")
        (KeliSymConst id _)        -> id
        other -> error (show other)
    

type KeliSymTab = OMap String KeliSymbol

emptyKeliSymTab :: KeliSymTab
emptyKeliSymTab = empty


