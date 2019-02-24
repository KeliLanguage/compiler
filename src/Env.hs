module Env where

import Text.Parsec.Pos
import Prelude hiding (id)
import Data.Map.Ordered 

import qualified Ast.Verified as V

data KeliSymbol
    = KeliSymFunc 
        [V.FuncSignature]

    | KeliSymConst          
        V.StringToken -- this field is used for enhancing DuplicatedIdentifiers error message
        V.Type

    | KeliSymType           
        V.Type 

    | KeliSymTaggedUnion 
        V.TaggedUnion
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
                KeliSymTaggedUnion 
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
        contextNextInt      :: Int,
        contextEnv          :: Env,
        contextImportedEnvs :: [Env]
    }

emptyContext :: Context
emptyContext = Context 0 emptyEnv []