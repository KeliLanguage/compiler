module StaticError where 

import Ast

data KeliError 
    = KErrorDuplicatedId -- StringToken StringToken
    | KErrorUsingUndefinedId
    | KErrorUsingUndefinedFunc