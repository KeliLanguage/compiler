{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagnostics where

import Prelude hiding (id)
import Text.ParserCombinators.Parsec
import GHC.Generics
import Data.Aeson
import Data.List
import Unify

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
        message     :: String
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
    KErrorAmbiguousUsage actualIds _ ->
        getDiagnostic actualIds "Ambiguous usage"

    KErrorUnknownProp x ->
        getDiagnostic [x] ("Unknown property: " ++ backtick (snd x))

    KErrorMoreThanOneElseBranch xs ->
        getDiagnostic xs "Should not have more than one `else` branch."

    KErrorTVarSelfReferencing{} ->
        undefined

    KErrorExpectedColon x ->
        getDiagnostic [x] "Expected this to be a `:` symbol."

    KErrorUnknownTag t ->
        getDiagnostic [t] ("Unknown tag: " ++ backtick (snd t))

    KErrorBindingCarrylessTag x ->
        getDiagnostic [x] "Carryless tag does not contains value to be binded to variables."

    KErrorExpectedTagBindings x ->
        getDiagnostic [x] "Expected this to be a tag binding expression."

    KErrorExpectedKeywordCase x ->
        getDiagnostic [x] "Expected this to be the keyword `case`."

    KErrorExpectedPropDefOrId x ->
        getDiagnostic [x] "Expected this to be properties definition or an identifier."

    KErrorExpectedTypeAnnotationAfterThis x ->
        getDiagnostic [x] "Expected a type annotation after this."
        
    KErrorExpectedKeywordCaseOrDefault x ->
        getDiagnostic [x] "Expected this to be the keyword `case` or `default`"

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
        [Diagnostic 1 (buildRange positionOfDotOperator 1) "Expecting function identifier after this dot operator."]
        
    KErrorParseError sp messages ->
        let pos1 = toPosition sp in
        let pos2 = pos1 {character = character pos1 + 1} in
        [Diagnostic 1 (Range pos1 pos2) (show messages)]
    
    KErrorDuplicatedId ids ->
        getDiagnostic ids "Duplicated identifier."

    KErrorDuplicatedProperties ids ->
        getDiagnostic ids "Duplicated properties."
    
    KErrorDuplicatedTags ids ->
        getDiagnostic ids "Duplicated tags."

    KErrorExcessiveTags tags tagUnionName ->
        map (\t -> Diagnostic 1 (getRange t) ("Excessive tag: " ++ intercalate " " [snd tagUnionName] ++ " does not have the tag " ++ init (snd t))) tags

    KErrorExcessiveProperties props ->
        getDiagnostic props "Excessive property."

    KErrorIncorrectCarryType expectedCarryType expr ->
        typeMismatchError expr expectedCarryType

    KErrorIncorrectUsageOfRecord id ->
        getDiagnostic [id] ("Incorrect usage of record.")
    
    KErrorIncorrectUsageOfTag id ->
        getDiagnostic [id] ("Incorrect usage of tag.")

    KErrorIncorrectUsageOfFFI id ->
        getDiagnostic [id] ("Incorrect usage of ffi.")

    KErrorMissingTags (V.Expr expr _) missingTags ->
        getDiagnostic [expr] ("Missing tags: " ++ intercalate ", " missingTags)

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

    KErrorCannotRedefineReservedConstant token ->
        getDiagnostic [token] ("Cannot redefined reserved constant " ++ backtick (snd token))

    KErrorCannotDefineCustomPrimitiveType token ->
        getDiagnostic [token] ("Cannot define custome primitive type: " ++ backtick (snd token))

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
            map (\id -> Diagnostic 1 (getRange id) errorMsg) strtoks

        showFuncIds :: [V.StringToken] -> String
        showFuncIds funcIds = backtick (intercalate " " (map snd funcIds))

        
backtick :: String -> String
backtick str = "`" ++ str ++ "`"

class HaveRange a where
    getRange :: a -> Range

instance HaveRange V.StringToken where
    getRange (sourcePos, str) = buildRange sourcePos (length str) 

instance HaveRange V.Expr where
    getRange (V.Expr expr _) = getRange expr

instance HaveRange V.TypeAnnotation where
    getRange (V.TypeAnnotSimple x _) = getRange x
    getRange (V.TypeAnnotCompound name keyTypeAnnotPairs _) =
        mergeRanges ([getRange name] ++ map (\(_,t) -> getRange t) keyTypeAnnotPairs)

instance HaveRange V.UnlinkedTag where
    getRange (V.UnlinkedCarrylessTag x) = getRange x
    getRange (V.UnlinkedCarryfulTag name keyTypeAnnotPairs) = 
        mergeRanges ([getRange name] ++ map (\(_,t) -> getRange t) keyTypeAnnotPairs)

instance HaveRange V.Expr' where
    getRange expression = case expression of
        V.PartiallyInferredLambda param body ->
            mergeRanges [getRange param, getRange body]

        V.RecordLambdaSetter subject _ _ lambdaBody ->
            mergeRanges [getRange subject, getRange lambdaBody]

        V.FFIJavascript code ->
            getRange code

        V.TagConstructorPrefix name ->
            getRange name

        V.TypeConstructorPrefix name ->
            getRange name

        V.CarryfulTagConstructor name _ ->
            getRange name

        V.RecordConstructor (Just name) _ ->
            getRange name

        V.RecordConstructor Nothing kvs ->
            getRange (fst (kvs !! 0))
        
        V.CarrylessTagExpr _ x _ ->
            getRange x

        V.CarryfulTagExpr name x _ ->
            mergeRanges [getRange name, getRange (PropValuePairs x)]

        V.TagMatcher subject branches _ ->
            mergeRanges [
                getRange subject,
                mergeRanges (map getRange branches)
            ]

        V.RecordSetter subject prop newValue ->
            mergeRanges [getRange subject, getRange prop, getRange newValue]

        V.RecordGetter subject prop ->
            mergeRanges [getRange subject, getRange prop]

        V.Lambda (param,_) body ->
            mergeRanges [getRange param, getRange body]

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

        V.FuncCall params ids _ ->
            let ranges1 = map (\(V.Expr expr' _) -> getRange expr') params in
            let ranges2 = map getRange ids in
            mergeRanges [mergeRanges ranges1, mergeRanges ranges2]

        V.Record x ->
            getRange (PropValuePairs x)

        V.FuncApp left right ->
            mergeRanges [getRange left, getRange right]

data PropValuePairs = PropValuePairs [(V.StringToken, V.Expr)]

instance HaveRange PropValuePairs where
    getRange (PropValuePairs propValuePairs) = 
        foldl' 
            (\ranges (prop, value) -> 
                mergeRanges [ranges ,mergeRanges [getRange prop, getRange value]])
            (let (prop,value) = head propValuePairs in mergeRanges [getRange prop, getRange value])
            (tail propValuePairs)

instance HaveRange Raw.Expr where
    getRange raw = case raw of
        Raw.NumberExpr (sourcePos, Left value) ->
            buildRange sourcePos (length (show value))

        Raw.NumberExpr (sourcePos, Right value) ->
            buildRange sourcePos (length (show value))

        Raw.StringExpr (sourcePos, value) ->
            buildRange sourcePos (length value + 2)
        
        Raw.Id (sourcePos, value) ->
            buildRange sourcePos (length value)

        Raw.FuncCall params ids ->
            mergeRanges (map getRange ids ++ map getRange params)


instance HaveRange V.TagBranch where
    getRange (V.CarrylessTagBranch tagname expr) = 
        mergeRanges [getRange tagname, getRange expr]
        
    getRange (V.CarryfulTagBranch tagname _ expr) =
        mergeRanges [getRange tagname, getRange expr]

    getRange (V.ElseBranch expr) = 
        getRange expr

instance HaveRange V.VerifiedTagname where
    getRange (V.VerifiedTagname x) = getRange x

buildRange :: SourcePos -> Int -> Range
buildRange sourcePos length' = 
    let from' = toPosition sourcePos in
    let to' = from' {character = character from' + length'} in
    Range from' to'

mergeRanges :: [Range] -> Range
mergeRanges ranges = 
    let sortedRanges = sort ranges in
    let minRange = head sortedRanges in
    let maxRange = last sortedRanges in
    Range (start minRange) (end maxRange)

instance ToJSON Diagnostic where
instance ToJSON Range where
instance ToJSON Position where