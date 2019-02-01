{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagnostics where

import Prelude hiding (id)
import Text.ParserCombinators.Parsec
import GHC.Generics
import Data.Aeson
import Data.List
import Analyzer
import TypeCheck

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

    KErrorExcessiveTags tags (_,tagUnionName) ->
        map (\t -> Diagnostic 1 (getRange t) ("Excessive tag: " ++ tagUnionName ++ " does not have the tag " ++ init (snd t))) tags

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

    KErrorNotAllBranchHaveTheSameType branches ->
        getDiagnostic branches "Not all branches have the same type."

    KErrorUnmatchingFuncReturnType body expectedType ->
        typeMismatchError body expectedType

    KErrorUsingUndefinedFunc funcIds possibleFuncs ->
        getDiagnostic funcIds ("Unknown function: " ++ showFuncIds funcIds ++ ".")

    KErrorUsingUndefinedId id ->
        getDiagnostic [id] ("Unknown indentifier: " ++ snd id)

    KErrorWrongTypeInSetter expr expectedType ->
        typeMismatchError expr expectedType

    KErrorPropertyTypeMismatch propname expectedType expr ->
        typeMismatchError expr expectedType

    KErrorNotAFunction funcIds ->
        getDiagnostic funcIds (showFuncIds funcIds ++ " is not a function")

    KErrorDuplicatedFunc func ->
        let funcIds = V.funcDeclIds func in
        getDiagnostic funcIds ("Duplicated function.")

    KErrorFuncCallTypeMismatch expectedType expr ->
        typeMismatchError expr expectedType


    where 
        typeMismatchError :: V.Expr -> V.Type -> [Diagnostic]
        typeMismatchError expr expectedType = 
            getDiagnostic [expr] ("Expected " ++ quote (V.stringifyType expectedType) ++ " but got " ++ quote (V.stringifyType (getType expr)))

        getDiagnostic :: HaveRange a => [a] -> String -> [Diagnostic]
        getDiagnostic strtoks errorMsg =
            map (\id -> Diagnostic 1 (getRange id) errorMsg) strtoks

        showFuncIds :: [V.StringToken] -> String
        showFuncIds funcIds = quote (intercalate " " (map snd funcIds))

        
quote :: String -> String
quote str = "`" ++ str ++ "`"

class HaveRange a where
    getRange :: a -> Range

instance HaveRange V.StringToken where
    getRange (sourcePos, str) = buildRange sourcePos (length str) 

instance HaveRange V.Expr where
    getRange (V.Expr expr _) = getRange expr

instance HaveRange V.Expr' where
    getRange expr = case expr of
        V.IntExpr (sourcePos, value) ->
            buildRange sourcePos (length (show value))

        V.DoubleExpr (sourcePos, value) ->
            buildRange sourcePos (length (show value))

        V.StringExpr (sourcePos, value) ->
            buildRange sourcePos (length value + 2)
        
        V.Id (sourcePos, value) ->
            buildRange sourcePos (length value)

        V.FuncCall params ids _ ->
            let ranges1 = map (\(V.Expr expr' _) -> getRange expr') params in
            let ranges2 = map getRange ids in
            mergeRanges [mergeRanges ranges1, mergeRanges ranges2]

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