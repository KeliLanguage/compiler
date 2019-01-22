{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Ast.Raw as Raw

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

keliParser :: Parser [Raw.Decl]
keliParser = whiteSpace >> keliDecl

keliDecl :: Parser [Raw.Decl]
keliDecl = do
    list <- (keliDecl' `endBy1` (symbol ";"))
    eof
    return list

keliDecl' :: Parser Raw.Decl
keliDecl' 
    =  try keliFuncDecl
   <|> keliConstDecl

keliConstDecl :: Parser Raw.Decl
keliConstDecl 
    =  optionMaybe (keliFuncId)   >>= \token
    -> reservedOp "="             >>= \_
    -> keliExpr                   >>= \expr
    -> case token of 
        Just t  -> return (Raw.ConstDecl (Raw.Const t expr Nothing))
        Nothing -> return (Raw.IdlessDecl expr)

keliExpr :: Parser Raw.Expr
keliExpr 
    =  try keliFuncCall
   <|> try keliLambda
   <|> keliAtomicExpr 

keliFuncCall :: Parser Raw.Expr
keliFuncCall 
    =  keliAtomicExpr     >>= \param1
    -> char '.' >> spaces >>= \_
    -> keliFuncCallTail   >>= \chain
    -> let pairs          = (flattenFuncCallChain chain) in
       let firstChain     = head pairs in
       let remainingChain = tail pairs in
        return (foldl' 
            (\acc next -> (Raw.FuncCall (acc : Raw.funcCallParams next) (Raw.funcCallIds next) Nothing)) -- reducer
            (Raw.FuncCall (param1:(snd firstChain)) (fst firstChain) Nothing)               -- initial value
            (map (\x -> Raw.FuncCall (snd x) (fst x) Nothing) remainingChain) -- foldee
        )

keliLambda :: Parser Raw.Expr
keliLambda
    =  many1 keliFuncId >>= \params
    -> reservedOp "|"   >>= \_
    -> keliExpr         >>= \expr
    -> return (Raw.Lambda params expr)

data KeliFuncCallChain
    = KeliFuncCallChain KeliFuncCallChain KeliFuncCallChain
    | KeliPartialFuncCall {
        partialFuncCallIds    :: [Raw.StringToken],
        partialFuncCallParams :: [Raw.Expr]
    }

flattenFuncCallChain :: KeliFuncCallChain -> [([Raw.StringToken], [Raw.Expr])]
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

keliAtomicExpr :: Parser Raw.Expr
keliAtomicExpr 
    =  parens keliExpr
   <|> (getPosition >>= \pos -> number     >>= \n   -> return (Raw.Number (pos, n)))
   <|> (getPosition >>= \pos -> keliFuncId >>= \id  -> return (Raw.Id id))
   <|> (getPosition >>= \pos -> stringLit  >>= \str -> return (Raw.String (pos, str)))

keliFuncDecl :: Parser Raw.Decl
keliFuncDecl 
    =  try keliPolyFuncDecl
   <|> keliMonoFuncDecl

keliMonoFuncDecl :: Parser Raw.Decl
keliMonoFuncDecl
    =  keliGenericParams  >>= \genparams
    -> keliFuncDeclParam  >>= \param
    -> char '.' >> spaces >>= \_ 
    -> keliFuncId         >>= \token
    -> reservedOp "|"     >>= \_
    -> keliExpr           >>= \typeExpr
    -> reservedOp "="     >>= \_
    -> keliExpr           >>= \expr
    -> return (Raw.FuncDecl (Raw.Func(unpackMaybe genparams) [param] [token] (Raw.TypeUnverified typeExpr) expr))

keliPolyFuncDecl :: Parser Raw.Decl
keliPolyFuncDecl   
    =  keliGenericParams  >>= \genparams
    -> keliFuncDeclParam  >>= \param1
    -> char '.' >> spaces >>= \_ 
    -> keliIdParamPair    >>= \xs
    -> reservedOp "|"     >>= \_
    -> keliExpr           >>= \typeExpr
    -> reservedOp "="     >>= \_
    -> keliExpr           >>= \expr
    -> return (Raw.FuncDecl (Raw.Func(unpackMaybe genparams) (param1:(map snd xs)) (map fst xs) (Raw.TypeUnverified typeExpr) expr))

unpackMaybe :: Maybe [a] -> [a]
unpackMaybe (Just x) = x
unpackMaybe Nothing  = []


braces  = between (symbol "{") (symbol "}")
keliGenericParams :: Parser (Maybe [Raw.FuncDeclConstraint])
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

keliFuncDeclParam ::Parser (Raw.StringToken, Raw.Type)
keliFuncDeclParam 
    =  keliFuncId     >>= \id
    -> reservedOp ":" >>= \_
    -> keliAtomicExpr >>= \typeExpr
    -> return (id, Raw.TypeUnverified typeExpr)

preprocess :: String -> String
preprocess str = str
    -- just in case we need it in the future:
    -- let packed = T.pack str in
    -- T.unpack (T.replace "\n\n" "\n;;;\n" packed)

parseKeli :: String -> Either KeliError [Raw.Decl] 
parseKeli input = 
    case parse keliParser "" (preprocess input) of
        Right decls -> Right decls
        Left err -> Left (KErrorParseError err)
