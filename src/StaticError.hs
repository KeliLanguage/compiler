module StaticError where 

import Ast

data KeliError 
    = KErrorDuplicatedId -- StringToken StringToken
    | KErrorUsingUndefinedId StringToken
    | KErrorUsingUndefinedFunc
    | KErrorIncorrectUsageOfRecord StringToken
    | KErrorUsingUndefinedType 
    deriving(Show)