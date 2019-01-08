module StaticError where 

import Ast

data KeliError 
    = KErrorDuplicatedId StringToken
    | KErrorUsingUndefinedId StringToken
    | KErrorUsingUndefinedFunc
    | KErrorIncorrectUsageOfRecord StringToken
    | KErrorUsingUndefinedType 
    | KErrorUnmatchingFuncReturnType
    deriving(Show)