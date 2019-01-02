{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Ast
import Lexer

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Text as T
import Data.List
import Debug.Trace

keliParser :: Parser KeliDecl
keliParser = whiteSpace >> keliDecl

keliDecl :: Parser KeliDecl
keliDecl = do
    list <- (sepEndBy1 keliDecl' (many1 (symbol ";;;")))
    eof
    return (Seq list)

keliDecl' :: Parser KeliDecl
keliDecl' 
    =  try keliFuncDecl
   <|> keliConstDecl

keliConstDecl :: Parser KeliDecl
keliConstDecl 
    =  optionMaybe identifier     >>= \id
    -> reservedOp "="             >>= \_
    -> keliExpr                   >>= \expr
    -> return (KeliConstDecl id expr Nothing)

keliExpr :: Parser KeliExpr
keliExpr 
    =  try keliFuncCall
   <|> keliAtomicExpr 

keliFuncCall :: Parser KeliExpr
keliFuncCall 
    =  keliAtomicExpr     >>= \param1
    -> char ',' >> spaces >>= \_
    -> keliFuncCallTail   >>= \chain
    -> let pairs          = (flattenFuncCallChain chain) in
       let firstChain     = head pairs in
       let remainingChain = tail pairs in
        return (foldl 
            (\acc next -> (KeliFuncCall (acc : funcCallParams next) (funcCallIds next))) -- reducer
            (KeliFuncCall (param1:(snd firstChain)) (fst firstChain))               -- initial value
            (map (\x -> KeliFuncCall (snd x) (fst x)) remainingChain) -- foldee
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
    = buildExpressionParser [[Infix (char ',' >> spaces >> return KeliFuncCallChain) AssocLeft]] keliPartialFuncCall

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
    =  keliFuncDeclParam  >>= \param
    -> char ',' >> spaces >>= \_ 
    -> keliFuncId         >>= \id
    -> reservedOp "->"    >>= \_
    -> keliExpr           >>= \typeExpr
    -> reservedOp "="     >>= \_
    -> keliExpr           >>= \expr
    -> return (KeliFuncDecl [param] [id] typeExpr expr)

keliPolyFuncDecl :: Parser KeliDecl
keliPolyFuncDecl   
    =  keliFuncDeclParam  >>= \param1
    -> char ',' >> spaces >>= \_ 
    -> keliIdParamPair    >>= \xs
    -> reservedOp "->"    >>= \_
    -> keliExpr           >>= \typeExpr
    -> reservedOp "="     >>= \_
    -> keliExpr           >>= \expr
    -> return (KeliFuncDecl (param1:(map snd xs)) (map fst xs) typeExpr expr)

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
preprocess str = 
    let packed = T.pack str in
    T.unpack (T.replace "\n\n" "\n;;;\n" packed)

parseKeli :: String -> Either ParseError KeliDecl 
parseKeli input = parse keliParser "" (preprocess input)