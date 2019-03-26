{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagnostics where

import Prelude hiding (id)
import Text.ParserCombinators.Parsec
import GHC.Generics
import Data.Aeson
import Data.Char
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
                            V.CarryfulTag (_,name) _ _ ->
                                "." ++ name ++ "(" ++ [toLower (head name)] ++ ")"

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

instance HaveRange V.StringToken where
    getRange (sourcePos, str) = buildRange sourcePos (length str) 
    getSourceName (sourcePos, _) = sourceName sourcePos

instance HaveRange V.Expr where
    getRange (V.Expr expr _) = getRange expr
    getSourceName (V.Expr expr _) = getSourceName expr

instance HaveRange V.TypeAnnotation where
    getRange (V.TypeAnnotSimple x _) = getRange x
    getRange (V.TypeAnnotCompound name keyTypeAnnotPairs _) =
        mergeRanges ([getRange name] ++ map (\(_,t) -> getRange t) keyTypeAnnotPairs)
    getRange (V.TypeAnnotObject keyTypePairs) = 
        mergeRanges(map (\(key,_) -> getRange key) keyTypePairs)


    getSourceName (V.TypeAnnotSimple x _) = getSourceName x
    getSourceName (V.TypeAnnotCompound name _ _) = getSourceName name
    getSourceName (V.TypeAnnotObject keyTypePairs) = getSourceName (fst (keyTypePairs !! 0))

instance HaveRange V.UnlinkedTag where
    getRange (V.UnlinkedCarrylessTag x) = getRange x
    getRange (V.UnlinkedCarryfulTag name typeAnnot) = 
        mergeRanges [getRange name, getRange typeAnnot]

    getSourceName (V.UnlinkedCarrylessTag name) = getSourceName name
    getSourceName (V.UnlinkedCarryfulTag name _) = getSourceName name

instance HaveRange V.Expr' where
    getRange expression = case expression of
        V.Array exprs ->
            mergeRanges (map getRange exprs)

        V.PartiallyInferredLambda param body ->
            mergeRanges [getRange param, getRange body]

        V.ObjectLambdaSetter subject _ _ lambdaBody ->
            mergeRanges [getRange subject, getRange lambdaBody]

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
            mergeRanges [getRange name, getRange carryExpr]


        V.TagMatcher subject branches _ ->
            mergeRanges [
                getRange subject,
                mergeRanges (map getRange branches)
            ]

        V.ObjectSetter subject prop newValue ->
            mergeRanges [getRange subject, getRange prop, getRange newValue]

        V.ObjectGetter subject prop ->
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

        V.Object x ->
            getRange (PropValuePairs x)

        V.FuncApp left right ->
            mergeRanges [getRange left, getRange right]


    getSourceName expression = case expression of
        V.Array exprs ->
            case exprs of
                [] ->
                    undefined

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
    getRange (PropValuePairs propValuePairs) = 
        foldl' 
            (\ranges (prop, value) -> 
                mergeRanges [ranges ,mergeRanges [getRange prop, getRange value]])
            (let (prop,value) = head propValuePairs in mergeRanges [getRange prop, getRange value])
            (tail propValuePairs)

    getSourceName (PropValuePairs propValuePairs) = 
        getSourceName (fst (propValuePairs !! 0))

instance HaveRange Raw.Expr where
    getRange raw = case raw of
        Raw.Array elems ->
            mergeRanges (map getRange elems)
        
        Raw.Lambda param body isShorthand ->
            if isShorthand then
                getRange body
            else
                mergeRanges [getRange param, getRange body]

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

    getSourceName raw = case raw of
        Raw.Array elems ->
            case elems of
                [] ->
                    undefined

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
        mergeRanges [getRange tagname, getRange expr]
        
    getRange (V.CarryfulTagBranch tagname _ expr) =
        mergeRanges [getRange tagname, getRange expr]

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

mergeRanges :: [Range] -> Range
mergeRanges ranges = 
    let sortedRanges = sort ranges in
    let minRange = head sortedRanges in
    let maxRange = last sortedRanges in
    Range (start minRange) (end maxRange)

instance ToJSON Diagnostic where
instance ToJSON Range where
instance ToJSON Position where