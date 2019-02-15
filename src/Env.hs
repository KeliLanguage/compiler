module Env where

import Text.Parsec.Pos
import Prelude hiding (id)
import Data.Map.Ordered 

import qualified Ast.Raw as Raw
import qualified Ast.Verified as Verified

data KeliSymbol
    = KeliSymFunc           [Verified.Func]
    | KeliSymConst          Raw.StringToken     Verified.Expr
    | KeliSymTag            Verified.Tag
    | KeliSymType           Verified.TypeAlias 
    | KeliSymTypeConstraint Raw.StringToken     Verified.TypeConstraint
    | KeliSymInlineExprs    [Verified.Expr] -- for storing expr from Raw.IdlessConst
    | KeliSymTypeConstructor Verified.TaggedUnion
        
    deriving(Show)

-- TODO: Verify the significance of this TypeClass
instance Verified.Identifiable KeliSymbol where
    getIdentifier sym = case sym of 
        (KeliSymType (Verified.TypeAlias id t)) -> (snd id, [id])
        (KeliSymTag t)              -> 
            case t of
                Verified.CarrylessTag x _   -> (snd x, [x])
                Verified.CarryfulTag  x _ _ -> (snd x, [x])
        (KeliSymConst id _)           -> (snd id, [id])
        (KeliSymTypeConstraint id _) -> (snd id, [id])
        (KeliSymTypeConstructor (Verified.TaggedUnion name _ _ _)) -> (snd name, [name])
        other -> error (show other)
    

type Env = OMap String KeliSymbol

emptyEnv :: Env
emptyEnv = empty


data Context 
    = Context 
        Int -- next integer of type variable
        Env

emptyContext :: Context
emptyContext = Context 0 emptyEnv
