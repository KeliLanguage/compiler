{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module StaticError where 


import Module
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
-- import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

import qualified Ast.Verified as Verified
import Env
import qualified Ast.Raw as Raw
import Util

data Messages = Messages [Message]

data KeliError 
    = KErrorParseError SourcePos Messages
    | KErrorDuplicatedId [Verified.StringToken]
    | KErrorDuplicatedProperties [Verified.StringToken]
    | KErrorDuplicatedTags [Verified.StringToken]
    | KErrorExcessiveTags 
        [Verified.StringToken] -- excessive tags
        Verified.StringToken -- name of tagged union
    | KErrorExcessiveProperties [Verified.StringToken]
    | KErrorIncorrectCarryType 
        Verified.Type -- expected type
        Verified.Expr -- actual expr
    | KErrorIncorrectUsageOfObject Verified.StringToken
    | KErrorIncorrectUsageOfTag Verified.StringToken
    | KErrorIncorrectUsageOfFFI Verified.StringToken
    | KErrorMissingTags 
        Verified.Expr -- subject
        [Verified.Tag] -- missing tags

    | KErrorMissingProperties 
        Verified.Expr' -- for telling where is the object constructor
        [String] -- missing props

    | KErrorUnmatchingFuncReturnType 
        Verified.Expr -- actual body
        Verified.Type -- expected type
    | KErrorUsingUndefinedFunc 
        [Verified.StringToken] -- function ids
        [Verified.FuncSignature] -- list of possible functions with the same ids

    | KErrorUsingUndefinedId Verified.StringToken
    | KErrorWrongTypeInSetter Verified.Expr Verified.Type
    | KErrorPropertyTypeMismatch
        Verified.StringToken -- property name
        Verified.Type  -- expected type
        Verified.Type  -- actual type
        Verified.Expr' -- actual expr 
    | KErrorNotAFunction [Verified.StringToken]
    | KErrorDuplicatedFunc Verified.FuncSignature
    | KErrorFuncCallTypeMismatch
        Verified.Type -- expected type
        Verified.Expr -- actual expr (type-checked)

    | KErrorCannotDeclareTypeAsAnonymousConstant Verified.TypeAnnotation
    | KErrorCannotDeclareTagAsAnonymousConstant [Verified.UnlinkedTag]

    | KErrorExpectedTypeAnnotButGotExpr      Verified.Expr
    | KErrorExpectedTagButGotExpr       Verified.Expr
    | KErrorExpectedTagButGotTypeAnnotation       Verified.TypeAnnotation
    | KErrorExpectedExprButGotTypeAnnotation      Verified.TypeAnnotation
    | KErrorExpectedTypeAnnotationButGotTag       [Verified.UnlinkedTag]
    | KErrorExpectedExprButGotTag       [Verified.UnlinkedTag]
    | KErrorExpectedExprOrTypeButGotTag [Verified.UnlinkedTag]
    | KErrorUnknownFFITarget Verified.StringToken
    | KErrorFFIValueShouldBeString Verified.Expr
    | KErrorInvalidBoundedTypeVarDecl Raw.Expr
    | KErrorIncorrectUsageOfTagConstructorPrefix Raw.Expr
    | KErrorTagNotFound 
        Raw.StringToken -- tag that user wanted to use
        Raw.StringToken -- name of the tagged union
        [Verified.Tag]    -- list of possible tags

    | KErrorIncompleteFuncCall -- for implementing Intellisense
        (OneOf3 Verified.Expr Verified.TypeAnnotation [Verified.UnlinkedTag])
        SourcePos -- position of the dot operator

    | KErrorCannotRedefineReservedConstant Raw.StringToken
    | KErrorCannotDefineCustomPrimitiveType Raw.StringToken
    | KErrorTypeConstructorIdsMismatch 
        [Raw.StringToken] -- expected ids
        [Raw.StringToken] -- actual ids

    | KErrorTypeMismatch 
        Verified.Expr' -- actual expr (for locating error position)
        Verified.Type  -- actual type
        Verified.Type  -- expected type

    | KErrorNotAllBranchHaveTheSameType 
        Verified.Expr' -- actual expr (for locating error position)
        Verified.Type  -- actual type
        Verified.Type  -- expected type
        Verified.TagBranch -- first branch

    | KErrorExpectedId
        Raw.Expr

    | KErrorExpectedKeywordIf 
        Raw.StringToken

    | KErrorExpectedPropDefOrId
        Raw.Expr

    | KErrorExpectedTypeAnnotationAfterThis
        Raw.StringToken

    | KErrorExpectedKeywordIfOrDefault
        Raw.StringToken

    | KErrorExpectedColon
        Raw.StringToken

    | KErrorUnknownTag
        Raw.StringToken

    | KErrorBindingCarrylessTag
        Raw.StringToken

    | KErrorExpectedTagBindings
        Raw.Expr

    | KErrorUnknownProp
        Raw.StringToken

    | KErrorMoreThanOneElseBranch
        [Raw.StringToken]

    | KErrorTVarSelfReferencing
        Verified.Expr'
        String
        Verified.Type

    | KErrorCannotImport
        Raw.StringToken

    | KErrorAmbiguousUsage
        [Raw.StringToken] 
        [(Verified.Scope, KeliSymbol)]

    | KErrorPartiallyMatchedFuncFound KeliError

    | KErrorIncorrectTagDeclSyntax
        [Raw.StringToken]
    
    deriving (Show)

instance Show Messages where
    show (Messages msgs) = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" msgs



