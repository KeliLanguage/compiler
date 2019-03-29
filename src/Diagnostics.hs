{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagnostics where

import Prelude hiding (id, head, tail, last, init)
import Text.ParserCombinators.Parsec
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)
import GHC.Generics
import Data.Aeson
import Data.Char
import Data.List hiding (last, head)
import Unify
import Util

import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError

-- The format of PackedError is according to Microsoft Language Server Protocol (check Diagnostics for more info)
-- So, that integration with VSCode can be as easy as ABC

data Diagnostic = 
    Diagnostic {
        severity    :: Int, 
            -- 1 : Error
            -- 2 : Warning
            -- 3 : Information
            -- 4 : Hint
        range       :: Range,
        message     :: String,
        filename    :: String
        -- relatedInformation :: Maybe 
    }
    deriving (Show, Generic)

data Range = 
    Range {
        start :: Position,
        end   :: Position
    }
    deriving (Show, Generic, Eq, Ord)

data Position = 
    Position {
        line      :: Int,
        character :: Int -- a.k.a column
    }
    deriving (Show, Generic, Eq, Ord)
    

toPosition :: SourcePos -> Position
toPosition sp = Position (sourceLine sp - 1) (sourceColumn sp - 1) -- minus one is necessary because LSP dictates zero-based index

toDiagnostic :: KeliError -> [Diagnostic]
toDiagnostic err = case err of
    KErrorIncorrectTagDeclSyntax funcIds ->
        getDiagnostic funcIds ("Incorrect tag declaration syntax")

    KErrorPartiallyMatchedFuncFound e ->
        toDiagnostic e

    KErrorAmbiguousUsage actualIds _ ->
        getDiagnostic actualIds "Ambiguous usage"

    KErrorUnknownProp x ->
        getDiagnostic [x] ("Unknown property: " ++ backtick (snd x))

    KErrorMoreThanOneElseBranch xs ->
        getDiagnostic xs "Should not have more than one `else` branch."

    KErrorTVarSelfReferencing expr _ _ ->
        getDiagnostic [expr] "Self referencing type variables."

    KErrorExpectedColon x ->
        getDiagnostic [x] "Expected this to be a `:` symbol."

    KErrorUnknownTag t ->
        getDiagnostic [t] ("Unknown tag: " ++ backtick (snd t))

    KErrorBindingCarrylessTag x ->
        getDiagnostic [x] "Carryless tag does not contains value to be binded to variables."

    KErrorExpectedTagBindings x ->
        getDiagnostic [x] "Expected this to be a tag binding expression."

    KErrorExpectedKeywordIf x ->
        getDiagnostic [x] "Expected this to be the keyword `if`."

    KErrorExpectedPropDefOrId x ->
        getDiagnostic [x] "Expected this to be properties definition or an identifier."

    KErrorExpectedTypeAnnotationAfterThis x ->
        getDiagnostic [x] "Expected a type annotation after this."
        
    KErrorExpectedKeywordIfOrDefault x ->
        getDiagnostic [x] "Expected this to be the keyword `if` or `default`"

    KErrorInvalidBoundedTypeVarDecl x ->
        getDiagnostic [x] "Invalid bounded type variable."

    KErrorIncorrectUsageOfTagConstructorPrefix x ->
        getDiagnostic [x] "Incorrect usage of tag constructor prefix."

    KErrorExpectedId x ->
        getDiagnostic [x] "Expected an identifier."

    KErrorTagNotFound actualTagname (_,taggedUnionName) _ ->
        getDiagnostic [actualTagname] ("The tagged union " ++ taggedUnionName ++ " does not have the tag " ++ backtick (snd actualTagname))

    KErrorTypeConstructorIdsMismatch expectedIds actualIds ->
        let combine xs = backtick (intercalate "," (map (\(_,id) -> id) xs)) in
        getDiagnostic actualIds ("Expected type constructor ids is " ++ combine expectedIds ++ " but got " ++ combine actualIds)

    KErrorUnknownFFITarget x ->
        getDiagnostic [x] ("Unknown FFI target: " ++ backtick (snd x))

    KErrorFFIValueShouldBeString x ->
        getDiagnostic [x] "FFI value should be type of String."

    KErrorExpectedExprButGotTypeAnnotation typeAnnon ->
        getDiagnostic [typeAnnon] "Expected an expression but got a type annotation."

    KErrorExpectedTypeAnnotationButGotTag tags ->
        getDiagnostic tags "Expected an type annotation but got tags."

    KErrorExpectedExprButGotTag tags ->
        getDiagnostic tags "Expected an expression but got tags."

    KErrorExpectedExprOrTypeButGotTag tags ->
        getDiagnostic tags "Expected an expression or a type annotation but got tags."

    KErrorCannotDeclareTypeAsAnonymousConstant x -> 
        getDiagnostic [x] "Cannot declare type as anonymous constant."

    KErrorCannotDeclareTagAsAnonymousConstant tags ->
        getDiagnostic tags "Cannot declare tags as anonymous constant."

    KErrorExpectedTagButGotExpr x ->
        getDiagnostic [x] "Expected a tag but got an expression."

    KErrorExpectedTagButGotTypeAnnotation x ->
        getDiagnostic [x] "Expected a tag but got a type annotation."

    KErrorIncompleteFuncCall _ positionOfDotOperator ->
        [Diagnostic 1 (buildRange positionOfDotOperator 1) 
            "Expecting function identifier after this dot operator."
            (sourceName positionOfDotOperator)
            ]
        
    KErrorParseError sp messages ->
        let pos1 = toPosition sp in
        let pos2 = pos1 {character = character pos1 + 1} in
        [Diagnostic 1 (Range pos1 pos2) (show messages) (sourceName sp)]
    
    KErrorDuplicatedId ids ->
        getDiagnostic ids "Duplicated identifier."

    KErrorDuplicatedProperties ids ->
        getDiagnostic ids "Duplicated properties."
    
    KErrorDuplicatedTags ids ->
        getDiagnostic ids "Duplicated tags."

    KErrorExcessiveTags tags tagUnionName ->
        map (\t -> 
            Diagnostic 
                1 
                (getRange t) 
                ("Excessive tag: " ++ intercalate " " [snd tagUnionName] ++ " does not have the tag " ++ init (snd t))
                (sourceName (fst t)))
            tags

    KErrorExcessiveProperties props ->
        getDiagnostic props "Excessive property."

    KErrorIncorrectCarryType expectedCarryType expr ->
        typeMismatchError expr expectedCarryType

    KErrorIncorrectUsageOfObject id ->
        getDiagnostic [id] ("Incorrect usage of object.")
    
    KErrorIncorrectUsageOfTag id ->
        getDiagnostic [id] ("Incorrect usage of tag.")

    KErrorIncorrectUsageOfFFI id ->
        getDiagnostic [id] ("Incorrect usage of ffi.")

    KErrorMissingTags (V.Expr expr _) missingTags ->
        let missingTags' = 
                map 
                    (\t ->
                        case t of
                            V.CarryfulTag (_,name@(firstChar:_)) _ _ ->
                                "." ++ name ++ "(" ++ [toLower firstChar] ++ ")"

                            V.CarrylessTag (_,name) _ ->
                                "." ++ name)
                    missingTags in
        getDiagnostic [expr] ("Missing cases:\n  " ++ intercalate "\n  " missingTags')

    KErrorMissingProperties expr missingProps ->
        getDiagnostic [expr] ("Missing properties: " ++ intercalate ", " missingProps)

    KErrorUnmatchingFuncReturnType body expectedType ->
        typeMismatchError body expectedType

    KErrorUsingUndefinedFunc funcIds possibleFuncs ->
        getDiagnostic funcIds ("Unknown function: " ++ showFuncIds funcIds ++ ".")

    KErrorUsingUndefinedId id ->
        getDiagnostic [id] ("Unknown indentifier: " ++ snd id)

    KErrorWrongTypeInSetter expr expectedType ->
        typeMismatchError expr expectedType

    KErrorPropertyTypeMismatch propname expectedType actualType actualExpr ->
        getDiagnostic [propname] (
            "The expected type of " ++ backtick (snd propname) ++ " is " ++ V.stringifyType expectedType ++ ".\n" ++
            "But the given expression have type of " ++ V.stringifyType actualType)

    KErrorNotAFunction funcIds ->
        getDiagnostic funcIds (showFuncIds funcIds ++ " is not a function")

    KErrorDuplicatedFunc func ->
        let funcIds = V.funcDeclIds func in
        getDiagnostic funcIds ("Duplicated function.")

    KErrorFuncCallTypeMismatch expectedType expr ->
        typeMismatchError expr expectedType

    KErrorCannotRedefineReservedConstant name ->
        getDiagnostic [name] ("Cannot redefined reserved constant " ++ backtick (snd name))

    KErrorCannotDefineCustomPrimitiveType name ->
        getDiagnostic [name] ("Cannot define custome primitive type: " ++ backtick (snd name))

    KErrorTypeMismatch actualExpr actualType expectedType ->
        typeMismatchError (V.Expr actualExpr ( actualType)) ( expectedType)

    KErrorExpectedTypeAnnotButGotExpr expr ->
        getDiagnostic [expr] ("Expected a type annotation but got an expression.")

    KErrorCannotImport filePath ->
        getDiagnostic [filePath] ("Cannot import this file. Maybe it does not exist, or you don't have the right to access it.")

    KErrorNotAllBranchHaveTheSameType actualExpr actualType expectedType firstBranch ->
        let locationOfFirstBranch = 
                case firstBranch of
                    V.ElseBranch expr ->
                        getRange expr

                    V.CarrylessTagBranch _ expr ->
                        getRange expr

                    V.CarryfulTagBranch _ _ expr ->
                        getRange expr in

        getDiagnostic [actualExpr] ("The expected type of each branch is "
                ++ backtick (V.stringifyType expectedType)
                ++ " (based on the type of first branch at Line " ++ show (line (start locationOfFirstBranch) + 1) ++ ")\n"
                ++ "But this branch has type of " 
                ++ backtick (V.stringifyType actualType))

    where 
        typeMismatchError :: V.Expr -> V.Type -> [Diagnostic]
        typeMismatchError expr expectedType = 
            getDiagnostic [expr] ("Expected " ++ backtick (V.stringifyType expectedType) ++ " but got " ++ backtick (V.stringifyType (getType expr)))

        getDiagnostic :: HaveRange a => [a] -> String -> [Diagnostic]
        getDiagnostic strtoks errorMsg =
            map (\x -> 
                    Diagnostic 
                        1 
                        (getRange x) 
                        errorMsg 
                        (getSourceName x)) strtoks

        showFuncIds :: [V.StringToken] -> String
        showFuncIds funcIds = backtick (intercalate " " (map snd funcIds))

        
backtick :: String -> String
backtick str = "`" ++ str ++ "`"

class HaveRange a where
    getRange      :: a -> Range
    getSourceName :: a -> String

instance HaveRange SourcePos where
    getRange sourcePos = buildRange sourcePos (length "[]")
    getSourceName sourcePos = sourceName sourcePos

instance HaveRange V.StringToken where
    getRange (sourcePos, str) = buildRange sourcePos (length str) 
    getSourceName (sourcePos, _) = sourceName sourcePos

instance HaveRange V.Expr where
    getRange (V.Expr expr _) = getRange expr
    getSourceName (V.Expr expr _) = getSourceName expr

instance HaveRange V.TypeAnnotation where
    getRange (V.TypeAnnotSimple x _) = getRange x
    getRange (V.TypeAnnotCompound name keyTypeAnnotPairs _) =
        mergeRanges name (map snd keyTypeAnnotPairs)

    getRange (V.TypeAnnotObject ((firstKey,_):tailKeyTypePairs)) = 
        mergeRanges firstKey (map snd tailKeyTypePairs)

    getSourceName (V.TypeAnnotSimple x _) = getSourceName x
    getSourceName (V.TypeAnnotCompound name _ _) = getSourceName name
    getSourceName (V.TypeAnnotObject keyTypePairs) = getSourceName (fst (keyTypePairs !! 0))

instance HaveRange V.UnlinkedTag where
    getRange (V.UnlinkedCarrylessTag x) = getRange x
    getRange (V.UnlinkedCarryfulTag name typeAnnot) = 
        mergeRanges name [typeAnnot]

    getSourceName (V.UnlinkedCarrylessTag name) = getSourceName name
    getSourceName (V.UnlinkedCarryfulTag name _) = getSourceName name

instance HaveRange V.Expr' where
    getRange expression = case expression of
        V.Array exprs pos ->
            case exprs of
                -- if this array is empty
                [] ->
                    getRange pos

                headExpr:tailExprs ->
                    mergeRanges headExpr tailExprs

        V.PartiallyInferredLambda param body ->
            mergeRanges param [body]

        V.ObjectLambdaSetter subject _ _ lambdaBody ->
            mergeRanges subject [lambdaBody]

        V.FFIJavascript code ->
            getRange code

        V.TagConstructorPrefix name ->
            getRange name

        V.TypeConstructorPrefix name ->
            getRange name

        V.CarryfulTagConstructor name _ ->
            getRange name

        V.ObjectConstructor (Just name) _ ->
            getRange name

        V.ObjectConstructor Nothing kvs ->
            getRange (fst (kvs !! 0))
        
        V.CarrylessTagExpr _ x _ ->
            getRange x

        V.CarryfulTagExpr name carryExpr _ ->
            mergeRanges name [carryExpr]


        V.TagMatcher subject branches _ ->
            mergeRanges subject branches

        V.ObjectSetter subject _ newValue ->
            mergeRanges subject [newValue]

        V.ObjectGetter subject prop ->
            mergeRanges subject [prop]

        V.Lambda (param,_) body ->
            mergeRanges param [body]

        V.IntExpr (sourcePos, value) ->
            buildRange sourcePos (length (show value))

        V.DoubleExpr (sourcePos, value) ->
            buildRange sourcePos (length (show value))

        V.StringExpr (sourcePos, value) ->
            buildRange sourcePos (length value + 2)
        
        V.GlobalId (sourcePos, value) _ _ ->
            buildRange sourcePos (length value)

        V.LocalId (sourcePos, value) _ ->
            buildRange sourcePos (length value)

        V.FuncCall (firstParam:tailParams) _ _ ->
            mergeRanges firstParam tailParams

        V.Object x ->
            getRange (PropValuePairs x)

        V.FuncApp left right ->
            mergeRanges left [right]


    getSourceName expression = case expression of
        V.Array exprs pos ->
            case exprs of
                [] ->
                    sourceName pos

                x:_ ->
                    getSourceName x

        V.PartiallyInferredLambda param _ ->
            getSourceName param

        V.ObjectLambdaSetter subject _ _ _ ->
            getSourceName subject

        V.FFIJavascript code ->
            getSourceName code

        V.TagConstructorPrefix name ->
            getSourceName name

        V.TypeConstructorPrefix name ->
            getSourceName name

        V.CarryfulTagConstructor name _ ->
            getSourceName name

        V.ObjectConstructor (Just name) _ ->
            getSourceName name

        V.ObjectConstructor Nothing kvs ->
            getSourceName (fst (kvs !! 0))
        
        V.CarrylessTagExpr _ x _ ->
            getSourceName x

        V.CarryfulTagExpr name _ _ ->
            getSourceName name

        V.TagMatcher subject _ _ ->
            getSourceName subject

        V.ObjectSetter _ prop _ ->
            getSourceName prop

        V.ObjectGetter _ prop ->
            getSourceName prop

        V.Lambda (param,_) _ ->
            getSourceName param

        V.IntExpr (sourcePos, _) ->
            sourceName sourcePos

        V.DoubleExpr (sourcePos, _) ->
            sourceName sourcePos

        V.StringExpr (sourcePos, _) ->
            sourceName sourcePos
        
        V.GlobalId (sourcePos, _) _ _ ->
            sourceName sourcePos

        V.LocalId (sourcePos, _) _ ->
            sourceName sourcePos

        V.FuncCall params _ _ ->
            getSourceName (params !! 0)

        V.Object x ->
            getSourceName (PropValuePairs x)

        V.FuncApp left _ ->
            getSourceName left

data PropValuePairs = PropValuePairs [(V.StringToken, V.Expr)]

instance HaveRange PropValuePairs where
    getRange (PropValuePairs ((firstProp,_):tailPropValuePairs)) = 
        mergeRanges firstProp (map snd tailPropValuePairs)

    getSourceName (PropValuePairs propValuePairs) = 
        getSourceName (fst (propValuePairs !! 0))

instance HaveRange Raw.Expr where
    getRange raw = case raw of
        Raw.Array elems pos ->
            case elems of
                [] ->
                    getRange pos
                x:xs ->
                    mergeRanges x xs
        
        Raw.Lambda param body isShorthand ->
            if isShorthand then
                getRange body
            else
                mergeRanges param [body]

        Raw.NumberExpr (sourcePos, Left value) ->
            buildRange sourcePos (length (show value))

        Raw.NumberExpr (sourcePos, Right value) ->
            buildRange sourcePos (length (show value))

        Raw.StringExpr (sourcePos, value) ->
            buildRange sourcePos (length value + 2)
        
        Raw.Id (sourcePos, value) ->
            buildRange sourcePos (length value)

        Raw.FuncCall (firstParam:tailParams) _ ->
            mergeRanges firstParam tailParams

    getSourceName raw = case raw of
        Raw.Array elems pos ->
            case elems of
                [] ->
                    sourceName pos

                x:_ ->
                    getSourceName x
        
        Raw.Lambda param _ _ ->
            getSourceName param

        Raw.NumberExpr (sourcePos, _) ->
            sourceName sourcePos 

        Raw.StringExpr (sourcePos, _) ->
            sourceName sourcePos 
        
        Raw.Id (sourcePos, _) ->
            sourceName sourcePos 

        Raw.FuncCall _ ids ->
            getSourceName (ids !! 0)

instance HaveRange V.TagBranch where
    getRange (V.CarrylessTagBranch tagname expr) = 
        mergeRanges tagname [expr]
        
    getRange (V.CarryfulTagBranch tagname _ expr) =
        mergeRanges tagname [expr]

    getRange (V.ElseBranch expr) = 
        getRange expr

    getSourceName (V.CarrylessTagBranch tagname _) = 
        getSourceName tagname
        
    getSourceName (V.CarryfulTagBranch tagname _ _) =
        getSourceName tagname

    getSourceName (V.ElseBranch expr) = 
        getSourceName expr

instance HaveRange V.VerifiedTagname where
    getRange (V.VerifiedTagname x) = getRange x
    getSourceName (V.VerifiedTagname x) = getSourceName x

buildRange :: SourcePos -> Int -> Range
buildRange sourcePos length' = 
    let from' = toPosition sourcePos in
    let to' = from' {character = character from' + length'} in
    Range from' to'

mergeRanges :: HaveRange a => HaveRange b => a -> [b] -> Range
mergeRanges x xs = 
    let startRange = getRange x in
    let endRange = case last xs of
            Nothing ->
                startRange

            Just e ->
                getRange e in
    Range (start startRange) (end endRange)
            
        



instance ToJSON Diagnostic where
instance ToJSON Range where
instance ToJSON Position where