module StaticError where 

import qualified Ast.Raw as Raw
import Text.ParserCombinators.Parsec
import Symbol

data KeliError 
    = KErrorParseError ParseError
    | KErrorDuplicatedId [Raw.StringToken]
    | KErrorDuplicatedProperties
    | KErrorDuplicatedTags [Raw.StringToken]
    | KErrorExcessiveTags [Raw.StringToken]
    | KErrorExcessiveProperties [Raw.StringToken]
    | KErrorIncorrectCarryType 
        Raw.Type -- expected type
        Raw.Expr -- actual expr
    | KErrorIncorrectUsageOfRecord Raw.StringToken
    | KErrorIncorrectUsageOfTag Raw.Expr
    | KErrorIncorrectUsageOfTaggedUnion Raw.Expr
    | KErrorMissingTags [String]
    | KErrorMissingProperties [String]
    | KErrorNotAllBranchHaveTheSameType [Raw.Expr]
    | KErrorUnmatchingFuncReturnType Raw.Type Raw.Type
    | KErrorUsingUndefinedFunc 
        [Raw.StringToken] -- function ids
        [Raw.Func] -- list of possible functions with the same ids

    | KErrorUsingUndefinedId Raw.StringToken
    | KErrorUsingUndefinedType [Raw.StringToken]
    | KErrorWrongTypeInSetter
    | KErrorPropretyTypeMismatch
        Raw.StringToken -- property name
        Raw.Type    -- expected type
        Raw.Expr    -- actual expr (type-checked)
    | KErrorNotATypeConstraint KeliSymbol
    | KErrorExprIsNotAType Raw.Expr
    | KErrorTagIsNotAType Raw.Tag
    | KErrorNotAFunction KeliSymbol
    | KErrorDuplicatedFunc Raw.Func
    | KErrorTypeNotConformingConstraint Raw.Type Raw.Constraint
    | KErrorFuncCallTypeMismatch
        Raw.Type -- expected type
        Raw.Expr -- actual expr (type-checked)
    | KErrorInvalidTypeConstructorParam Raw.FuncDeclParam
    | KErrorInvalidParamLengthForGenericType 
        [Raw.Expr] -- applied params
        Int -- expected param length
    | KErrorBodyOfGenericTypeIsNotTypeDeclaration
        Raw.Expr -- actual body
    | KErrorCannotDeclareTypeAsAnonymousConstant Raw.Type
    | KErrorCannotDeclareTagAsAnonymousConstant Raw.Tag
    | KErrorTypeIsNotAnExpr Raw.Type
    | KErrorTagIsNotAnExpr Raw.Tag

    deriving(Show)