{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module StaticError where 


import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

import qualified Ast.Verified as Verified
import qualified Ast.Raw as Raw
import Symbol
import Util

data Messages = Messages [Message]

data KeliError 
    = KErrorParseError SourcePos Messages
    | KErrorDuplicatedId [Verified.StringToken]
    | KErrorDuplicatedProperties [Verified.StringToken]
    | KErrorDuplicatedTags [Verified.StringToken]
    | KErrorExcessiveTags 
        [Verified.StringToken] -- excessive tags
        [Verified.StringToken] -- name of tagged union
    | KErrorExcessiveProperties [Verified.StringToken]
    | KErrorIncorrectCarryType 
        Verified.Type -- expected type
        Verified.Expr -- actual expr
    | KErrorIncorrectUsageOfRecord Verified.StringToken
    | KErrorIncorrectUsageOfTag Verified.StringToken
    | KErrorIncorrectUsageOfFFI Verified.StringToken
    | KErrorMissingTags 
        Verified.Expr -- subject
        [String] -- missing tags

    | KErrorMissingProperties 
        Verified.Expr -- for telling where is the record constructor
        [String] -- missing props

    | KErrorNotAllBranchHaveTheSameType [Verified.Expr]
    | KErrorUnmatchingFuncReturnType 
        Verified.Expr -- actual body
        Verified.Type -- expected type
    | KErrorUsingUndefinedFunc 
        [Verified.StringToken] -- function ids
        [Verified.Func] -- list of possible functions with the same ids

    | KErrorUsingUndefinedId Verified.StringToken
    | KErrorWrongTypeInSetter Verified.Expr Verified.Type
    | KErrorPropertyTypeMismatch
        Verified.StringToken -- property name
        Verified.Type    -- expected type
        Verified.Expr    -- actual expr (type-checked)
    | KErrorNotAFunction [Verified.StringToken]
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
    | KErrorIncorrectMethodToRetrieveCarry Verified.StringToken
    | KErrorInvalidTypeParamDecl Raw.Expr
    | KErrorIncorrectUsageOfTagConstructorPrefix Raw.Expr
    | KErrorTagNotFound 
        [Raw.StringToken] -- tag that user wanted to use
        [Raw.StringToken] -- name of the tagged union
        [Verified.Tag]    -- list of possible tags

    | KErrorIncompleteFuncCall -- for implementing Intellisense
        (OneOf3 Verified.Expr Verified.Type Verified.Tag)
        SourcePos -- position of the dot operator

    | KErrorCannotRedefineReservedConstant Raw.StringToken
    | KErrorCannotDefineCustomPrimitiveType Raw.StringToken
    | KErrorTypeConstructorIdsMismatch [Raw.StringToken]
    
    deriving (Show)

instance Show Messages where
    show (Messages msgs) = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" msgs



