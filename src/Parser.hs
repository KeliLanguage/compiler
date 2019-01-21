{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Ast.Raw
import Lexer

import StaticError
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
import Data.Maybe
import Debug.Trace

keliParser :: Parser [KeliDecl]
keliParser = whiteSpace >> keliDecl

keliDecl :: Parser [KeliDecl]
keliDecl = do
    list <- (keliDecl' `endBy1` (symbol ";"))
    eof
    return list

keliDecl' :: Parser KeliDecl
keliDecl' 
    =  try keliFuncDecl
   <|> keliConstDecl

keliConstDecl :: Parser KeliDecl
keliConstDecl 
    =  optionMaybe (keliFuncId)   >>= \token
    -> reservedOp "="             >>= \_
    -> keliExpr                   >>= \expr
    -> case token of 
        Just t  -> return (KeliConstDecl (KeliConst t expr Nothing))
        Nothing -> return (KeliIdlessDecl expr)

keliExpr :: Parser KeliExpr
keliExpr 
    =  try keliFuncCall
   <|> try keliLambda
   <|> keliAtomicExpr 

keliFuncCall :: Parser KeliExpr
keliFuncCall 
    =  keliAtomicExpr     >>= \param1
    -> char '.' >> spaces >>= \_
    -> keliFuncCallTail   >>= \chain
    -> let pairs          = (flattenFuncCallChain chain) in
       let firstChain     = head pairs in
       let remainingChain = tail pairs in
        return (foldl' 
            (\acc next -> (KeliFuncCall (acc : funcCallParams next) (funcCallIds next) Nothing)) -- reducer
            (KeliFuncCall (param1:(snd firstChain)) (fst firstChain) Nothing)               -- initial value
            (map (\x -> KeliFuncCall (snd x) (fst x) Nothing) remainingChain) -- foldee
        )

keliLambda :: Parser KeliExpr
keliLambda
    =  many1 keliFuncId >>= \params
    -> reservedOp "|"   >>= \_
    -> keliExpr         >>= \expr
    -> return (KeliLambda params expr)

data KeliFuncCallChain
    = KeliFuncCallChain KeliFuncCallChain KeliFuncCallChain
    | KeliPartialFuncCall {
        partialFuncCallIds    :: [StringToken],
        partialFuncCallParams :: [KeliExpr]
    }

flattenFuncCallChain :: KeliFuncCallChain -> [([StringToken], [KeliExpr])]
flattenFuncCallChain (KeliFuncCallChain x y) = (flattenFuncCallChain x ++ flattenFuncCallChain y)
flattenFuncCallChain (KeliPartialFuncCall ids params) = [(ids, params)]

keliFuncCallTail :: Parser KeliFuncCallChain
keliFuncCallTail
    = buildExpressionParser [[Infix (char '.' >> spaces >> return KeliFuncCallChain) AssocLeft]] keliPartialFuncCall

keliPartialFuncCall
    -- binary/ternary/polynary
    = try ((many1 ( 
                    keliFuncId     >>= \token 
                 -> keliAtomicExpr >>= \expr
                 -> return (token, expr)
            )) >>= \pairs
            -> return (KeliPartialFuncCall (map fst pairs) (map snd pairs))
    )
    -- unary
   <|> (
            keliFuncId  >>= \token
        ->  return (KeliPartialFuncCall [token] []))

keliAtomicExpr :: Parser KeliExpr
keliAtomicExpr 
    =  parens keliExpr
   <|> (getPosition >>= \pos -> number     >>= \n   -> return (KeliNumber (pos, n)))
   <|> (getPosition >>= \pos -> keliFuncId >>= \id  -> return (KeliId id))
   <|> (getPosition >>= \pos -> stringLit  >>= \str -> return (KeliString (pos, str)))

keliFuncDecl :: Parser KeliDecl
keliFuncDecl 
    =  try keliPolyFuncDecl
   <|> keliMonoFuncDecl

keliMonoFuncDecl :: Parser KeliDecl
keliMonoFuncDecl
    =  keliGenericParams  >>= \genparams
    -> keliFuncDeclParam  >>= \param
    -> char '.' >> spaces >>= \_ 
    -> keliFuncId         >>= \token
    -> reservedOp "|"     >>= \_
    -> keliExpr           >>= \typeExpr
    -> reservedOp "="     >>= \_
    -> keliExpr           >>= \expr
    -> return (KeliFuncDecl (KeliFunc(unpackMaybe genparams) [param] [token] (KeliTypeUnverified typeExpr) expr))

keliPolyFuncDecl :: Parser KeliDecl
keliPolyFuncDecl   
    =  keliGenericParams  >>= \genparams
    -> keliFuncDeclParam  >>= \param1
    -> char '.' >> spaces >>= \_ 
    -> keliIdParamPair    >>= \xs
    -> reservedOp "|"     >>= \_
    -> keliExpr           >>= \typeExpr
    -> reservedOp "="     >>= \_
    -> keliExpr           >>= \expr
    -> return (KeliFuncDecl (KeliFunc(unpackMaybe genparams) (param1:(map snd xs)) (map fst xs) (KeliTypeUnverified typeExpr) expr))

unpackMaybe :: Maybe [a] -> [a]
unpackMaybe (Just x) = x
unpackMaybe Nothing  = []


braces  = between (symbol "{") (symbol "}")
keliGenericParams :: Parser (Maybe [KeliFuncDeclConstraint])
keliGenericParams 
    =  optionMaybe $ braces $ many1 (keliFuncDeclParam >>= \param ->  return param)


keliIdParamPair = 
    many1 (
            keliFuncId        >>= \token 
        ->  keliFuncDeclParam >>= \param
        -> return (token, param)
    )

keliFuncId = 
        getPosition                   >>= \pos 
    ->  choice [identifier, operator] >>= \id
    ->  return (pos, id)

keliFuncDeclParam ::Parser (StringToken, KeliType)
keliFuncDeclParam 
    =  keliFuncId     >>= \id
    -> reservedOp ":" >>= \_
    -> keliAtomicExpr >>= \typeExpr
    -> return (id, KeliTypeUnverified typeExpr)

preprocess :: String -> String
preprocess str = str
    -- just in case we need it in the future:
    -- let packed = T.pack str in
    -- T.unpack (T.replace "\n\n" "\n;;;\n" packed)

parseKeli :: String -> Either KeliError [KeliDecl] 
parseKeli input = 
    case parse keliParser "" (preprocess input) of
        Right decls -> Right decls
        Left err -> Left (KErrorParseError err)
