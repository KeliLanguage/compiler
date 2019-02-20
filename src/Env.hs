module Env where

import Text.Parsec.Pos
import Prelude hiding (id)
import Data.Map.Ordered 

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V

data KeliSymbol
    = KeliSymFunc           [V.Func]
    | KeliSymConst          Raw.StringToken     V.Expr
    | KeliSymType           V.Type 
    | KeliSymTypeConstructor V.TaggedUnion
    | KeliSymInlineExprs    [V.Expr] -- for storing expr from Raw.IdlessConst
        
    deriving(Show)

type Env = OMap String KeliSymbol

emptyEnv :: Env
emptyEnv = empty

builtinPos :: String -> V.StringToken
builtinPos str = (newPos "<builtin>" 0 0, str)

initialEnv :: Env
initialEnv = 
    empty 
        |> ("Int"   ,   KeliSymType V.TypeInt)
        |> ("Float" ,   KeliSymType V.TypeFloat)
        |> ("String",   KeliSymType V.TypeString)
        |> ("Type"  ,   KeliSymType V.TypeType)
        |> ("Function", 
                KeliSymTypeConstructor 
                    (newFunctionType 
                        (V.BoundedTypeVar (builtinPos "A") Nothing) 
                        (V.BoundedTypeVar (builtinPos "B") Nothing)))

newFunctionType :: V.Type -> V.Type -> V.TaggedUnion
newFunctionType inputType outputType =
    V.TaggedUnion 
        (builtinPos "Function")
        [builtinPos "in", builtinPos "out"]
        []
        [inputType, outputType]

data Context 
    = Context {
        contextNextInt :: Int,
        contextEnv :: Env
    }

emptyContext :: Context
emptyContext = Context 0 emptyEnv