module StaticError where 

import Ast
import Text.ParserCombinators.Parsec

data KeliError 
    = KErrorParseError ParseError
    | KErrorDuplicatedId StringToken
    | KErrorIncorrectUsageOfRecord StringToken
    | KErrorIncorrectUsageOfTag
    | KErrorIncorrectUsageOfTaggedUnion
    | KErrorUnmatchingFuncReturnType
    | KErrorUsingUndefinedFunc [StringToken]
    | KErrorUsingUndefinedId StringToken
    | KErrorUsingUndefinedType 
    | KErrorWrongTypeInSetter
    deriving(Show)