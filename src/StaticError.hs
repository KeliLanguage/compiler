module StaticError where 

import qualified Ast.Verified as Verified
import qualified Ast.Raw as Raw
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
    | KErrorIncorrectUsageOfFFI SourcePos
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
    | KErrorNotAFunction KeliSymbol
    | KErrorDuplicatedFunc Verified.Func
    | KErrorTypeNotConformingConstraint Verified.Type Verified.TypeConstraint
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

    | KErrorExpectedTypeButGotExpr      Verified.Expr
    | KErrorExpectedTagButGotExpr       Verified.Expr
    | KErrorExpectedTagButGotType       Verified.Type
    | KErrorExpectedExprButGotType      Verified.Type
    | KErrorExpectedTypeButGotTag       Verified.Tag
    | KErrorExpectedExprButGotTag       Verified.Tag
    | KErrorExpectedExprOrTypeButGotTag Verified.Tag
    | KErrorUnknownFFITarget Verified.StringToken
    | KErrorFFIValueShouldBeString Verified.Expr
    | KErrorExprIsNotATypeConstraint    Raw.Expr


    deriving(Show)