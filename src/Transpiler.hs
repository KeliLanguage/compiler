module Transpiler 
where

import Lib

class Transpilable a where
    transpile :: a -> String

instance Transpilable KeliDecl where
    transpile x = case x of 
        KeliConstDecl id expr -> "const " ++ id ++ "=" ++ (transpile expr)

instance Transpilable KeliExpr where
    transpile x = case x of 
        KeliNumber value -> show value