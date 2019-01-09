module StaticError where 

import Ast
import Text.ParserCombinators.Parsec

data KeliError 
    = KErrorParseError ParseError
    | KErrorDuplicatedId StringToken
    | KErrorUsingUndefinedId StringToken
    | KErrorUsingUndefinedFunc
    | KErrorIncorrectUsageOfRecord StringToken
    | KErrorUsingUndefinedType 
    | KErrorUnmatchingFuncReturnType
    deriving(Show)