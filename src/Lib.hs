{-# LANGUAGE OverloadedStrings #-}
module Lib where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Text as T
import Data.List
import Debug.Trace


data KeliDecl 
    = Seq [KeliDecl] -- Sequences of Declarations
    | KeliConstDecl { 
        constDeclId    :: String,
        constDeclValue :: KeliExpr,
        constDeclType  :: Maybe KeliExpr
    }
    | KeliFuncDecl {
        funcDeclParams     :: [KeliFuncDeclParam],
        funcDeclIds        :: [String],
        funcDeclReturnType :: KeliExpr
    }
    deriving (Show)

data KeliFuncDeclParam 
    = KeliFuncDeclParam {
        funcDeclParamId   :: String,
        funcDeclParamType :: KeliExpr
    }
    deriving (Show)

    

data KeliExpr 
    = KeliNumber (Either Integer Double)
    | KeliString String
    | KeliArray  [KeliExpr]
    | KeliTuple  [KeliExpr]
    | KeliId     String
    | KeliFuncCall {
        funcCallParams :: [KeliExpr],
        funcCallIds    :: [String]
    }
    | KeliLambda {
        lambdaParams :: [String],
        lambdaBody   :: KeliExpr
    }
    deriving (Show)

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedOpNames = [
                 "="
               , "->"
               , "."
               , ":"
               ]
           }

lexer = Token.makeTokenParser languageDef

-- Refer http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-ParserCombinators-Parsec-Token.html
identifier = Token.identifier     lexer -- parses an identifier
reserved   = Token.reserved       lexer -- parses a reserved name
reservedOp = Token.reservedOp     lexer -- parses an operator
parens     = Token.parens         lexer -- parses surrounding parenthesis:
                                        --   parens p
                                        -- takes care of the parenthesis and
                                        -- uses p to parse what's inside them
integer    = Token.integer        lexer -- parses an integer
float      = Token.float          lexer
number     = Token.naturalOrFloat lexer
semi       = Token.semi           lexer -- parses a semicolon
whiteSpace = Token.whiteSpace     lexer -- parses whitespace
symbol     = Token.symbol         lexer -- custom symbol
stringLit  = Token.stringLiteral  lexer
dot        = Token.dot            lexer
operator   = Token.operator       lexer

keliParser :: Parser KeliDecl
keliParser = whiteSpace >> keliDecl

keliDecl :: Parser KeliDecl
keliDecl = do
    list <- (sepBy1 keliDecl' (symbol ";"))
    return (Seq list)

keliDecl' :: Parser KeliDecl
keliDecl' 
    =  try keliFuncDecl
   <|> keliConstDecl

keliConstDecl :: Parser KeliDecl
keliConstDecl 
    =  identifier     >>= \id
    -> reservedOp "=" >>= \_
    -> keliExpr       >>= \expr
    -> return (KeliConstDecl id expr Nothing)

keliExpr :: Parser KeliExpr
keliExpr 
    =  try keliFuncCall
   <|> keliAtomicExpr 

keliFuncCall :: Parser KeliExpr
keliFuncCall 
    =  keliAtomicExpr   >>= \param1
    -> reservedOp "."   >>= \_
    -> keliFuncCallTail >>= \chain
    -> return $ trace (show (flattenFuncCallChain chain)) $ (foldl 
        (\acc next -> (KeliFuncCall (acc : funcCallParams next) (funcCallIds next))) -- reducer
        (KeliFuncCall [param1] [])               -- initial value
        (map (\x -> KeliFuncCall (snd x) (fst x)) (flattenFuncCallChain chain)) -- foldee
    )

data KeliFuncCallChain
    = KeliFuncCallChain KeliFuncCallChain KeliFuncCallChain
    | KeliPartialFuncCall {
        partialFuncCallIds    :: [String],
        partialFuncCallParams :: [KeliExpr]
    }

flattenFuncCallChain :: KeliFuncCallChain -> [([String], [KeliExpr])]
flattenFuncCallChain (KeliFuncCallChain x y) = (flattenFuncCallChain x ++ flattenFuncCallChain y)
flattenFuncCallChain (KeliPartialFuncCall ids params) = [(ids, params)]

keliFuncCallTail :: Parser KeliFuncCallChain
keliFuncCallTail
    = buildExpressionParser [[Infix (reservedOp "." >> return KeliFuncCallChain) AssocLeft]] keliPartialFuncCall

keliPartialFuncCall
    -- binary/ternary/polynary
    = try ((many1 ( keliFuncId     >>= \id 
                 -> keliAtomicExpr >>= \expr
                 -> return (id, expr)
            )) >>= \pairs
            -> return (KeliPartialFuncCall (map fst pairs) (map snd pairs))
    )
    -- unary
   <|> (keliFuncId >>= \id -> return (KeliPartialFuncCall [id] []))

keliAtomicExpr :: Parser KeliExpr
keliAtomicExpr 
    =  parens keliExpr
   <|> liftM KeliNumber number
   <|> liftM KeliId identifier
   <|> liftM KeliString stringLit

keliFuncDecl :: Parser KeliDecl
keliFuncDecl 
    =  try keliPolyFuncDecl
   <|> keliMonoFuncDecl

keliMonoFuncDecl :: Parser KeliDecl
keliMonoFuncDecl
    =  keliFuncDeclParam >>= \param
    -> reservedOp "."    >>= \_ 
    -> keliFuncId        >>= \id
    -> reservedOp "->"   >>= \_
    -> keliExpr          >>= \typeExpr
    -> reservedOp "="    >>= \_
    -> keliExpr          >>= \expr
    -> return (KeliFuncDecl [param] [id] typeExpr)

keliPolyFuncDecl :: Parser KeliDecl
keliPolyFuncDecl   
    =  keliFuncDeclParam >>= \param1
    -> reservedOp "."    >>= \_ 
    -> keliIdParamPair   >>= \xs
    -> reservedOp "->"   >>= \_
    -> keliExpr          >>= \typeExpr
    -> reservedOp "="    >>= \_
    -> keliExpr          >>= \expr
    -> return (KeliFuncDecl (param1:(map snd xs)) (map fst xs) typeExpr)

keliIdParamPair = 
    many1 (
            keliFuncId        >>= \id 
        ->  keliFuncDeclParam >>= \param
        -> return (id, param)
    )

keliFuncId = choice [identifier, operator]

keliFuncDeclParam :: Parser KeliFuncDeclParam
keliFuncDeclParam 
    =  identifier     >>= \id
    -> reservedOp ":" >>= \_
    -> keliAtomicExpr >>= \typeExpr
    -> return (KeliFuncDeclParam id typeExpr)

preprocess :: String -> String
preprocess str = T.unpack (T.replace "\n\n" ";" (T.pack str))

parseKeli :: String -> KeliDecl
parseKeli input =
    case parse keliParser "" (preprocess input) of
        Left  e -> error $ show e
        Right r -> r


debugParseKeli input =
    case parse (keliParser >> parserTrace "debug") "" (preprocess input) of
        Left  e -> error $ show e
        Right r -> r