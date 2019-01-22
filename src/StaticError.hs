module StaticError where 

import qualified Ast.Verified as Verified
import Text.ParserCombinators.Parsec
import Symbol

data KeliError 
    = KErrorParseError ParseError
    | KErrorDuplicatedId [Verified.StringToken]
    | KErrorDuplicatedProperties
    | KErrorDuplicatedTags [Verified.StringToken]
    | KErrorExcessiveTags [Verified.StringToken]
    | KErrorExcessiveProperties [Verified.StringToken]
    | KErrorIncorrectCarryType 
        Verified.Type -- expected type
        Verified.Expr -- actual expr
    | KErrorIncorrectUsageOfRecord Verified.StringToken
    | KErrorIncorrectUsageOfTag SourcePos
    | KErrorIncorrectUsageOfTaggedUnion Verified.Expr
    | KErrorMissingTags [String]
    | KErrorMissingProperties [String]
    | KErrorNotAllBranchHaveTheSameType [Verified.Expr]
    | KErrorUnmatchingFuncReturnType Verified.Type Verified.Type
    | KErrorUsingUndefinedFunc 
        [Verified.StringToken] -- function ids
        [Verified.Func] -- list of possible functions with the same ids

    | KErrorUsingUndefinedId Verified.StringToken
    | KErrorUsingUndefinedType [Verified.StringToken]
    | KErrorWrongTypeInSetter
    | KErrorPropretyTypeMismatch
        Verified.StringToken -- property name
        Verified.Type    -- expected type
        Verified.Expr    -- actual expr (type-checked)
    | KErrorNotATypeConstraint KeliSymbol
    | KErrorExprIsNotAType Verified.Expr
    | KErrorTagIsNotAType Verified.Tag
    | KErrorNotAFunction KeliSymbol
    | KErrorDuplicatedFunc Verified.Func
    | KErrorTypeNotConformingConstraint Verified.Type Verified.Constraint
    | KErrorFuncCallTypeMismatch
        Verified.Type -- expected type
        Verified.Expr -- actual expr (type-checked)
    | KErrorInvalidTypeConstructorParam Verified.FuncDeclParam
    | KErrorInvalidParamLengthForGenericType 
        [Verified.Expr] -- applied params
        Int -- expected param length
    | KErrorBodyOfGenericTypeIsNotTypeDeclaration
        Verified.Expr -- actual body
    | KErrorCannotDeclareTypeAsAnonymousConstant Verified.Type
    | KErrorCannotDeclareTagAsAnonymousConstant Verified.Tag
    | KErrorTypeIsNotAnExpr Verified.Type
    | KErrorTagIsNotAnExpr Verified.Tag
    | KErrorExprIsNotATagOrUnion Verified.Expr
    | KErrorTypeIsNotATagOrUnion Verified.Type

    deriving(Show)