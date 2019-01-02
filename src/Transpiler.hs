module Transpiler 
where

import Ast

class Transpilable a where
    transpile :: a -> String

instance Transpilable KeliDecl where
    transpile x = case x of 
        KeliConstDecl (Just id) expr _ -> "const " ++ id ++ "=" ++ (transpile expr)

instance Transpilable KeliExpr where
    transpile x = case x of 
        KeliNumber value -> show value