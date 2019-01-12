module StaticError where 

import Ast
import Text.ParserCombinators.Parsec

data KeliError 
    = KErrorParseError ParseError
    | KErrorDuplicatedId StringToken
    | KErrorDuplicatedProperties
    | KErrorDuplicatedTags [StringToken]
    | KErrorExcessiveTags [StringToken]
    | KErrorExcessiveProperties [StringToken]
    | KErrorIncorrectCarryType
        KeliType -- expected type
        KeliExpr -- actual expr
    | KErrorIncorrectUsageOfRecord StringToken
    | KErrorIncorrectUsageOfTag
    | KErrorIncorrectUsageOfTaggedUnion
    | KErrorMissingTags [String]
    | KErrorMissingProperties [String]
    | KErrorNotAType KeliExpr
    | KErrorNotAllBranchHaveTheSameType [(StringToken,KeliExpr)]
    | KErrorUnmatchingFuncReturnType KeliType KeliType
    | KErrorUsingUndefinedFunc [StringToken]
    | KErrorUsingUndefinedId StringToken
    | KErrorUsingUndefinedType KeliExpr
    | KErrorWrongTypeInSetter
    | KErrorPropretyTypeMismatch
        StringToken -- property name
        KeliType    -- expected type
        KeliExpr    -- actual expr (type-checked)
    deriving(Show)