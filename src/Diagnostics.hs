{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagnostics where

import Prelude hiding (id)
import Text.ParserCombinators.Parsec
import GHC.Generics
import Data.Aeson

import qualified Ast.Verified as Verified
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
    deriving (Show, Generic)

data Position = 
    Position {
        line      :: Int,
        character :: Int -- a.k.a column
    }
    deriving (Show, Generic)

toPosition :: SourcePos -> Position
toPosition sp = Position (sourceLine sp - 1) (sourceColumn sp - 1) -- minus one is necessary because LSP dictates zero-based index

toDiagnostic :: KeliError -> [Diagnostic]
toDiagnostic err = case err of
    KErrorParseError sp messages ->
        let pos1 = toPosition sp in
        let pos2 = pos1 {character = character pos1 + 1} in
        [Diagnostic 1 (Range pos1 pos2) (show messages)]
    
    KErrorDuplicatedId ids ->
        map (\id -> Diagnostic 1 (getRange id) "Duplicated identifier.") ids

    KErrorDuplicatedProperties duplicates ->
        map (\id -> Diagnostic 1 (getRange id) "Duplicated properties.") duplicates

class HaveRange a where
    getRange :: a -> Range

instance HaveRange Verified.StringToken where
    getRange (sourcePos, str) = 
        let from' = toPosition sourcePos in
        let to' = from' {character = character from' + length str} in
        Range from' to'

instance ToJSON Diagnostic where
instance ToJSON Range where
instance ToJSON Position where