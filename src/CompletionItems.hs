{-# LANGUAGE DeriveGeneric #-}

module CompletionItems where

import GHC.Generics
import Data.Aeson
import Symbol
import Data.List

import qualified Ast.Verified as V

-- The interface is based on CompletionItem described in https://microsoft.github.io/language-server-protocol/specification

data CompletionItem = CompletionItem {
    label  :: String,
    kind   :: Int
    {-
        export declare namespace CompletionItemKind {
            const Text: 1;
            const Method: 2;
            const Function: 3;
            const Constructor: 4;
            const Field: 5;
            const Variable: 6;
            const Class: 7;
            const Interface: 8;
            const Module: 9;
            const Property: 10;
            const Unit: 11;
            const Value: 12;
            const Enum: 13;
            const Keyword: 14;
            const Snippet: 15;
            const Color: 16;
            const File: 17;
            const Reference: 18;
            const Folder: 19;
            const EnumMember: 20;
            const Constant: 21;
            const Struct: 22;
            const Event: 23;
            const Operator: 24;
            const TypeParameter: 25;
        }
    -}
} deriving (Show, Generic)

instance ToJSON CompletionItem where

toCompletionItem :: KeliSymbol -> [CompletionItem]
toCompletionItem symbol = 
    case symbol of 
        KeliSymConst (_, id) _ -> 
            [CompletionItem id 6]
        
        KeliSymType (V.TypeAlias ids _) ->
            [CompletionItem (intercalate " " (map snd ids)) 8]
        
        KeliSymFunc funcs -> 
            map 
                (\f -> 
                    let ids = V.funcDeclIds f in
                    CompletionItem (intercalate " " (map snd ids)) 3
                ) 
                funcs
        _ -> 
            []