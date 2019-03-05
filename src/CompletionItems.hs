{-# LANGUAGE DeriveGeneric #-}

module CompletionItems where

import GHC.Generics
import Module
import Parser
import Data.Sequence(fromList, update, index)
import Data.Aeson
import Data.Foldable
import Data.Char
import Env
import Compiler
import Analyzer
import Util
import Data.List
import qualified Ast.Raw as Raw
import qualified Ast.Verified as V
import TypeCheck
import Prelude hiding(id)
import Unify
import StaticError(KeliError(KErrorIncompleteFuncCall))
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)


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
    insertTextFormat :: Int, -- 1 = Plain Text, 2 = Snippet
                            -- For snippet format, refer https://github.com/Microsoft/vscode/blob/master/src/vs/editor/contrib/snippet/snippet.md

    documentation :: String
} deriving (Show, Generic, Eq, Read)

instance ToJSON CompletionItem where


toCompletionItem ::KeliSymbol -> [CompletionItem]
toCompletionItem symbol = 
    case symbol of 
        KeliSymGlobalConst (_, id) _ -> 
            [CompletionItem  6 id  "Constant" id 1 ""]
        
        KeliSymType t ->
            let id = V.stringifyType t in
            [CompletionItem 7 id "Type" id 1 ""]

        KeliSymTaggedUnion (V.TaggedUnion (_,id) ids _ _) ->
            [CompletionItem 7 id "Tagged union type constructor" id 1 ""]
        
        KeliSymFunc funcs -> 
            concatMap 
                (\f ->
                    let ids = V.funcDeclIds f in 
                    let funcParams = V.funcDeclParams f in
                    let signature = (intercalate "() " (map snd ids)) in
                    let label' = 
                            (if length funcParams > 1 then 
                                signature ++ "()"
                            else
                                signature) in

                    let text = 
                            (if length funcParams == 1 then 
                                signature
                            else
                                makeKeyValuesSnippet (zip (map snd ids) (map (snd . fst) (tail funcParams)))) in
                    [CompletionItem 3
                        label'
                        (rebuildSignature f)
                        text
                        2 
                        (case V.funcDeclDocString f of Just doc -> doc; Nothing -> "")])
                funcs

makeKeyValuesSnippet :: [(String,String)] -> String
makeKeyValuesSnippet kvs =
    intercalate " "
    (map 
        (\((key, value), index') -> 
            key ++ "(${" ++ show index' ++ ":" ++ value ++ "})")
        (zip kvs [1..]))


rebuildSignature :: V.FuncSignature-> String 
rebuildSignature (V.FuncSignature _ genparams params funcIds returnType) = 
    let 
        front = stringifyFuncParam (head params) ++ "." 
        back = " | " ++ V.stringifyType returnType 
    in
    if length funcIds == 1 && length params == 1 then
        front ++ snd (head funcIds) ++ back 
    else
        front ++ intercalate " " (map (\(funcId, param) -> snd funcId ++ stringifyFuncParam param) (zip funcIds (tail params))) ++ back



stringifyFuncParam :: (V.StringToken, V.TypeAnnotation) -> String
stringifyFuncParam ((_,paramName), paramTypeAnnot) = 
    bracketize (paramName ++ " " ++ stringifyTypeAnnot paramTypeAnnot)

stringifyTypeAnnot :: V.TypeAnnotation -> String
stringifyTypeAnnot (V.TypeAnnotSimple (_,name) _) = name
stringifyTypeAnnot (V.TypeAnnotCompound (_,name) keyTypeAnnotPairs _) = 
    name ++ "." ++ intercalate " " (map (\(key, ta) -> snd key ++ "(" ++ stringifyTypeAnnot ta ++ ")") keyTypeAnnotPairs)
    
bracketize :: String -> String
bracketize str = "(" ++ str ++ ")"

suggestCompletionItemsAt 
    :: String -- filename
    -> (Int,Int) -- (lineNumber, columnNumber)
    -> IO [CompletionItem]

suggestCompletionItemsAt filename (lineNumber, columnNumber) = do
    contents <- readFile filename
    let lines' = lines contents 
    let currentChar = lines' !! lineNumber !! columnNumber 
    let modifiedContents = 
            -- if the current character is a dot(.)
                -- replace it with semicolon(;)
                -- so that we can parse KeliIncompleteFuncCall properly
            if currentChar == '.' then
                let lines'' = fromList (map fromList lines') in
                let result = (
                        update 
                            -- at
                            lineNumber 

                            -- with new value
                            (update 
                                -- at
                                columnNumber

                                -- with new value
                                ';'

                                -- over
                                (lines'' `index` lineNumber))

                            -- over
                            lines'') in

                intercalate "\n" (map toList (toList (result)))
            else
                contents

    (errors, currentModule) <- keliCompile filename modifiedContents
    case keliParse filename modifiedContents of
        Right decls ->
            let envs = [moduleEnv currentModule] ++ map moduleEnv (moduleImported currentModule) in
            let items = suggestCompletionItems envs errors in


            -- remove duplicates
            return (nubBy (\x y -> label x == label y) items)

        Left err ->
            return []


suggestCompletionItems :: [Env] -> [KeliError] -> [CompletionItem]
suggestCompletionItems importedEnvs errors =
    let symbols = concatMap extractSymbols importedEnvs in
    case find (\e -> case e of KErrorIncompleteFuncCall{} -> True; _ -> False) errors of
        Just (KErrorIncompleteFuncCall thing positionOfDotOperator) -> 
            -- suggest functions
            suggestCompletionItems' importedEnvs symbols (Just thing)

        _ ->
            -- suggest identifiers
            suggestCompletionItems' importedEnvs symbols Nothing


-- suggest completion item based on `subjectExpr`
suggestCompletionItems' :: [Env] -> [KeliSymbol] -> Maybe (OneOf3 V.Expr V.TypeAnnotation [V.UnlinkedTag]) -> [CompletionItem]
suggestCompletionItems' importedEnvs symbols subjectExpr  = case subjectExpr of
    -- if not triggered by pressing the dot operator
    Nothing ->
        -- then only return only non-functions identifiers
        concatMap 
            toCompletionItem 
            ((filter (\d -> case d of KeliSymFunc{} -> False; _ -> True) (symbols)))

    -- if is triggered by pressing the dot operator
    Just thing ->
        case thing of
            First expr -> 
                let relatedFuncs =
                        concatMap 
                        (\s -> 
                            case s of 
                                KeliSymFunc funcs ->
                                    concatMap
                                        (\f -> 
                                            let (_,firstParamTypeAnnon) = V.funcDeclParams f !! 0 in
                                            -- instantiate type variables first
                                            let (_, subst) = instantiateTypeVar (Context 999 emptyEnv []) (V.funcDeclGenericParams f) in
                                            case unify expr (applySubstitutionToType subst (V.getTypeRef firstParamTypeAnnon)) of
                                                Right _ ->
                                                    [KeliSymFunc [f]]
                                                Left _ ->
                                                    [])
                                        funcs
                                _ -> 
                                    [])
                        symbols in

                let relatedFuncsCompletionItems = concatMap toCompletionItem relatedFuncs in

                case expr of
                    -- tag constructor prefix
                    V.Expr _ (V.TypeTagConstructorPrefix _ tags typeParams _) ->
                        map 
                            (\t -> 
                                case t of
                                    V.CarryfulTag (_,tagname) propTypePairs _ ->
                                        let text = tagname ++ "." ++ intercalate " "
                                                        (map (\((_,key),t') -> key ++ "(" ++ V.stringifyType t' ++ ")") propTypePairs) in
                                            CompletionItem {
                                            kind = 13, -- enum
                                            label = text,
                                            detail = "",
                                            insertText = tagname 
                                                ++ "." 
                                                ++ makeKeyValuesSnippet (map (\(p,t') -> (snd p, V.stringifyType t')) propTypePairs),
                                            insertTextFormat = 2,
                                            documentation = ""
                                        }
                                    
                                    V.CarrylessTag (_,tagname) _ ->
                                        CompletionItem {
                                            kind = 13, -- enum
                                            label = tagname,
                                            detail = "",
                                            insertText = tagname,
                                            insertTextFormat = 1,
                                            documentation = ""
                                        })
                            tags 

                    -- object constructor
                    V.Expr _ (V.TypeObjectConstructor name propTypePairs) -> 
                        let text = concat (map (\((_,prop), t) -> prop ++ "(" ++ V.stringifyType t ++ ") ") propTypePairs) in
                        [CompletionItem {
                            kind = 4, -- constructor
                            label = text,
                            detail = "constructor",
                            insertText = makeKeyValuesSnippet (map (\(p, t) -> (snd p, V.stringifyType t)) propTypePairs),
                            insertTextFormat = 2,
                            documentation = ""
                        }]


                    -- lambda
                    V.Expr f (V.TypeTaggedUnion (V.TaggedUnion (_,"Function") _ _ _)) ->
                        case f of
                            V.Lambda ((_,paramName), _) lambdaBody ->
                                -- If the paramName contains no alphabet, then it must be an auto-generated name
                                -- implying that it is a lambda shorthand
                                if all (not . isAlpha) paramName then
                                    -- is shorthand lambda
                                    -- this is kind of a hack, but it works
                                    -- give suggestion item based on the lambda body
                                    suggestCompletionItems' importedEnvs symbols (Just (First lambdaBody))
                                else
                                    defaultResult

                            _ ->
                                defaultResult

                            where
                                defaultResult = 
                                    -- is normal lambda
                                    [CompletionItem {
                                        kind = 2,
                                        label = "apply",
                                        detail = "",
                                        insertText = "apply($1)",
                                        insertTextFormat = 2,
                                        documentation = ""
                                    }]

                    -- tag matchers
                    V.Expr _ (V.TypeTaggedUnion (V.TaggedUnion _ _ tags _)) ->
                        let insertText' = 
                                concatMap 
                                    (\(t,index) -> "\n\t" ++ 
                                        (case t of 
                                            V.CarryfulTag (_,tagname) expectedPropTypePairs _ ->
                                                "case(" ++ tagname ++ "."
                                                    ++ intercalate " " (map (\((_,prop), _) -> prop ++ bracketize ([toLower (head prop)])) expectedPropTypePairs)
                                                    ++ "):\n\t\t(${" ++ show index ++ ":undefined})"

                                            V.CarrylessTag (_,tagname) _ ->
                                                "case(" ++ tagname ++ "):\n\t\t(${" ++ show index ++ ":undefined})")) 
                                    (zip tags [1..]) in
                        [CompletionItem {
                            kind = 12, -- value
                            label = "case(...)",
                            detail = "tag matcher",
                            insertText = insertText',
                            insertTextFormat = 2,
                            documentation = ""
                        }] ++ relatedFuncsCompletionItems

                    -- object (getter/setter)
                    V.Expr _ (V.TypeObject _ propTypePairs) ->
                        (concatMap 
                            (\((_,prop), expectedType') ->
                                    [CompletionItem {
                                        kind = 10, -- property
                                        label = prop,
                                        detail = "getter",
                                        insertText = prop,
                                        insertTextFormat = 1,
                                        documentation = ""
                                    },
                                    CompletionItem {
                                        kind = 10, -- property
                                        label = prop ++ "()",
                                        detail = "setter",
                                        insertText = prop ++ "(${1:undefined})",
                                        insertTextFormat = 2,
                                        documentation = ""
                                    }])
                            propTypePairs) ++ relatedFuncsCompletionItems



                    --     -- otherwise: scope related functions
                    _ ->
                        relatedFuncsCompletionItems

            Third tag ->
                []

            _ ->
                [CompletionItem {
                    kind = 1,
                    label = "Gotcha",
                    detail = "Declare a carryful tag.",
                    insertText = "(tag.#(${1:tagName}) carry(${2:carryType}))",
                    insertTextFormat = 1,
                    documentation = ""
                }]

