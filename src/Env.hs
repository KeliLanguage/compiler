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
    | KeliSymType           Verified.Type 
    | KeliSymTypeConstraint Raw.StringToken     Verified.TypeConstraint
    | KeliSymInlineExprs    [Verified.Expr] -- for storing expr from Raw.IdlessConst
    | KeliSymTypeConstructor Verified.TaggedUnion
        
    deriving(Show)

type Env = OMap String KeliSymbol

emptyEnv :: Env
emptyEnv = empty

initialEnv :: Env
initialEnv = 
    empty 
        |> ("Int"   , KeliSymType Verified.TypeInt)
        |> ("Float" , KeliSymType Verified.TypeFloat)
        |> ("String", KeliSymType Verified.TypeString)
        |> ("Type"  , KeliSymType Verified.TypeType)


data Context 
    = Context {
        contextNextInt :: Int,
        contextEnv :: Env
    }

emptyContext :: Context
emptyContext = Context 0 emptyEnv