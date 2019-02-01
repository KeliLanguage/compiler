{-# LANGUAGE DeriveGeneric #-}

module CompletionItems where

import GHC.Generics
import Data.Aeson
import Symbol
import Analyzer
import Util
import Data.List
import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import StaticError(KeliError(KErrorIncompleteFuncCall))


import qualified Ast.Verified as V

-- The interface is based on CompletionItem described in https://microsoft.github.io/language-server-protocol/specification

data CompletionItem = CompletionItem {
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
    -},
    label  :: String,
    detail:: String,
    insertText :: String, -- text to be inserted if user chose this completion item
    insertTextFormat :: Int -- 1 = Plain Text, 2 = Snippet
                            -- For snippet format, refer https://github.com/Microsoft/vscode/blob/master/src/vs/editor/contrib/snippet/snippet.md
} deriving (Show, Generic)

instance ToJSON CompletionItem where

toCompletionItem :: KeliSymbol -> [CompletionItem]
toCompletionItem symbol = 
    case symbol of 
        KeliSymConst (_, id) _ -> 
            [CompletionItem  6 id  "" id 1]
        
        KeliSymType (V.TypeAlias ids _) ->
            let text = intercalate " " (map snd ids) in
            [CompletionItem 8 text "" text 1]
        
        KeliSymFunc funcs -> 
            map 
                (\f -> 
                    let ids = V.funcDeclIds f in 
                    let funcParams = V.funcDeclParams f in
                    let signature = (intercalate "() " (map snd ids)) in
                    let text = 
                            (if length funcParams > 1 then 
                                signature ++ "()"
                            else
                                signature) in
                    CompletionItem 3
                        text
                        (rebuildSignature f)
                        text
                        1 -- TODO: change to 2 (snippet)
                ) 
                funcs
        _ -> 
            []


rebuildSignature :: V.Func -> String 
rebuildSignature (V.Func genparams params funcIds returnType _) = 
    let 
        front = stringifyFuncParam (head params) ++ "." 
        back = " | " ++ V.stringifyType returnType 
    in
    if length funcIds == 1 && length params == 1 then
        front ++ snd (head funcIds) ++ back 
    else
        front ++ intercalate " " (map (\(funcId, param) -> snd funcId ++ stringifyFuncParam param) (zip funcIds (tail params))) ++ back



stringifyFuncParam :: V.FuncDeclParam -> String
stringifyFuncParam ((_,paramName), paramType) = 
    bracketize (paramName ++ " " ++ V.stringifyType paramType)
    
bracketize :: String -> String
bracketize str = "(" ++ str ++ ")"


suggestCompletionItems :: [Raw.Decl] -> [CompletionItem]
suggestCompletionItems decls =
    let (errors,_,symbols) = analyzeDecls emptyKeliSymTab decls in
    case find (\e -> case e of KErrorIncompleteFuncCall{} -> True; _ -> False) errors of
        Just (KErrorIncompleteFuncCall thing positionOfDotOperator) -> 
                            
            case thing of
                First (V.Expr expr exprType) -> 
                    let relatedFuncs =
                            map 
                            (\s -> 
                                case s of 
                                    KeliSymFunc funcs -> 
                                        filter 
                                            (\f -> 
                                                let (_,firstParamType) = V.funcDeclParams f !! 0 in
                                                exprType `V.typeEquals` firstParamType) 
                                            funcs
                                    _ -> 
                                        [])
                            symbols in

                    let relatedFuncsCompletionItems = concat (map toCompletionItem [KeliSymFunc (concat relatedFuncs)]) in

                    case exprType of
                        -- tag constructor prefix
                        V.TypeTagConstructorPrefix _ tags ->
                            map 
                                (\t -> 
                                    case t of
                                        V.CarryfulTag (_,tagname) carryType _ ->
                                            CompletionItem {
                                                kind = 13, -- enum
                                                label = tagname,
                                                detail = "",
                                                insertText = tagname ++ ".carry(${1:" ++ V.stringifyType carryType ++ "})",
                                                insertTextFormat = 2
                                            }
                                        
                                        V.CarrylessTag (_,tagname) _ ->
                                            CompletionItem {
                                                kind = 13, -- enum
                                                label = tagname,
                                                detail = "",
                                                insertText = tagname,
                                                insertTextFormat = 1
                                            })
                                tags 

                        -- record constructor
                        V.TypeRecordConstructor propTypePairs -> 
                            let text = concat (map (\((_,prop), _) -> prop ++ "() ") propTypePairs) in
                            [CompletionItem {
                                kind = 4, -- constructor
                                label = text,
                                detail = "constructor",
                                insertText = text,
                                insertTextFormat = 1
                            }]

                        -- tag matchers
                        V.TypeTagUnion _ tags ->
                            [CompletionItem {
                                kind = 2, -- method
                                label = concat (map (\t -> snd (V.tagnameOf t) ++ "? ") tags),
                                detail = "tag matcher",
                                insertText = concat (map (\t -> "\n\t" ++ snd (V.tagnameOf t) ++ "? ()") tags),
                                insertTextFormat = 1
                            }] ++ relatedFuncsCompletionItems

                        -- record (getter/setter)
                        V.TypeRecord propTypePairs ->
                            (map 
                                (\((_,prop), expectedType') ->
                                        CompletionItem {
                                            kind = 10, -- property
                                            label = prop,
                                            detail = V.stringifyType expectedType',
                                            insertText = prop,
                                            insertTextFormat = 1
                                        })
                                propTypePairs) ++ relatedFuncsCompletionItems

                        -- TODO: lambda (apply)


                        -- otherwise: scope related functions
                        _ ->
                            relatedFuncsCompletionItems

                Third tag ->
                    [CompletionItem {
                        kind = 14, -- keyword
                        label = "or()",
                        detail = "This function is used for constructing tagged unions.",
                        insertText = "or(${1:tag})",
                        insertTextFormat = 2 -- snippet
                    }]

                _ ->
                    [CompletionItem {
                        kind = 1,
                        label = "Gotcha",
                        detail = "Declare a carryful tag.",
                        insertText = "(tag.#(${1:tagName}) carry(${2:carryType}))",
                        insertTextFormat = 1
                    }]

        _ ->
            -- if not triggered by pressing the dot operator
            -- then only return only non-functions identifiers
            concat (map toCompletionItem (filter (\s -> case s of KeliSymFunc{} -> False; _ -> True) symbols))