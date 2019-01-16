module StaticError where 

import Ast
import Text.ParserCombinators.Parsec
import Symbol

data KeliError 
    = KErrorParseError ParseError
    | KErrorDuplicatedId [StringToken]
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
    | KErrorNotAllBranchHaveTheSameType [(StringToken,KeliExpr)]
    | KErrorUnmatchingFuncReturnType KeliType KeliType
    | KErrorUsingUndefinedFunc 
        [StringToken] -- function ids
        [KeliFunc] -- list of possible functions with the same ids

    | KErrorUsingUndefinedId StringToken
    | KErrorUsingUndefinedType KeliExpr
    | KErrorWrongTypeInSetter
    | KErrorPropretyTypeMismatch
        StringToken -- property name
        KeliType    -- expected type
        KeliExpr    -- actual expr (type-checked)
    | KErrorNotATypeConstraint KeliSymbol
    | KErrorNotAType KeliSymbol
    | KErrorDuplicatedFunc KeliFunc
    | KErrorTypeNotConformingConstraint KeliType KeliConstraint
    | KErrorFuncCallTypeMismatch
        KeliType -- expected type
        KeliExpr -- actual expr (type-checked)

    deriving(Show)