module StaticError where 

import Ast
import Text.ParserCombinators.Parsec

data KeliError 
    = KErrorParseError ParseError
    | KErrorDuplicatedId StringToken
    | KErrorIncorrectUsageOfRecord StringToken
    | KErrorIncorrectUsageOfTag
    | KErrorIncorrectUsageOfTaggedUnion
    | KErrorUnmatchingFuncReturnType KeliType KeliType
    | KErrorUsingUndefinedFunc [StringToken]
    | KErrorUsingUndefinedId StringToken
    | KErrorWrongTypeInSetter
    | KErrorExcessiveTags [StringToken]
    | KErrorMissingTags [String]
    | KErrorNotAllBranchHaveTheSameType [StringToken]
    | KErrorDuplicatedTags [StringToken]
    | KErrorNotAType KeliExpr
    | KErrorUsingUndefinedType KeliExpr
    deriving(Show)