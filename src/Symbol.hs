module Symbol where

import Text.Parsec.Pos
import Data.Map.Ordered 

import qualified Ast.Raw as Raw
import qualified Ast.Verified as Verified

data KeliSymbol
    = KeliSymFunc           [Verified.Func]
    | KeliSymConst          Raw.StringToken     Verified.Expr
    | KeliSymTag            Verified.Tag
    | KeliSymType           Verified.TypeAlias
    | KeliSymTypeParam      Raw.StringToken     Verified.TypeConstraint
    | KeliSymTypeConstraint Raw.StringToken     Verified.TypeConstraint
    | KeliSymInlineExprs    [Verified.Expr] -- for storing expr from Raw.IdlessConst
    deriving(Show)

-- TODO: Verify the significance of this TypeClass
instance Verified.Identifiable KeliSymbol where
    getIdentifier sym = case sym of 
        (KeliSymType (Verified.TypeAlias ids t)) -> (concat (map snd ids), ids)
        (KeliSymTag t)              -> 
            case t of
                Verified.CarrylessTag x _   -> (snd x, [x])
                Verified.CarryfulTag  x _ _ -> (snd x, [x])
        (KeliSymConst id _)           -> (snd id, [id])
        (KeliSymTypeConstraint id _) -> (snd id, [id])
        other -> error (show other)
    

type KeliSymTab = OMap String KeliSymbol

emptyKeliSymTab :: KeliSymTab
emptyKeliSymTab = empty


