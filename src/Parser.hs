{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Prelude hiding (id)

import qualified Ast.Raw as Raw

import Lexer

import StaticError
import Text.ParserCombinators.Parsec hiding (token)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error 
import Data.List

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
        Just t  -> return (Raw.ConstDecl (Raw.Const t expr))
        Nothing -> return (Raw.IdlessDecl expr)

keliExpr :: Parser Raw.Expr
keliExpr 
    =  try keliFuncCall
   <|> try keliLambda
   <|> try keliTypeAnnotatedExpr
   <|> keliAtomicExpr 

keliTypeAnnotatedExpr :: Parser Raw.Expr
keliTypeAnnotatedExpr 
    =   keliAtomicExpr  >>= \expr
    ->  reservedOp ":"  >>= \_
    ->  keliAtomicExpr  >>= \annotatedType
    -> return (Raw.AnnotatedExpr expr annotatedType)

keliFuncCall :: Parser Raw.Expr
keliFuncCall 
    =  keliAtomicExpr     >>= \param1
    -> char '.' >> spaces >>= \_
    -> keliFuncCallTail   >>= \chain
    -> let pairs          = (flattenFuncCallChain chain) in
       let firstChain     = head pairs in
       let remainingChain = tail pairs in
        return (foldl' 
            (\acc next -> (Raw.FuncCall (acc : Raw.funcCallParams next) (Raw.funcCallIds next))) -- reducer
            (Raw.FuncCall (param1:(snd firstChain)) (fst firstChain))               -- initial value
            (map (\x -> Raw.FuncCall (snd x) (fst x)) remainingChain) -- foldee
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
   <|> (getPosition >>= \pos -> number     >>= \n   -> return (Raw.NumberExpr (pos, n)))
   <|> (                        keliFuncId >>= \id  -> return (Raw.Id id))
   <|> (getPosition >>= \pos -> stringLit  >>= \str -> return (Raw.StringExpr (pos, str)))

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
    -> return (Raw.FuncDecl (Raw.Func(unpackMaybe genparams) [param] [token] typeExpr expr))

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
    -> return (Raw.FuncDecl (Raw.Func(unpackMaybe genparams) (param1:(map snd xs)) (map fst xs) typeExpr expr))

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

keliFuncDeclParam ::Parser (Raw.StringToken, Raw.Expr)
keliFuncDeclParam 
    =  keliFuncId     >>= \id
    -> reservedOp ":" >>= \_
    -> keliAtomicExpr >>= \typeExpr
    -> return (id, typeExpr)

preprocess :: String -> String
preprocess str = str
    -- just in case we need it in the future:
    -- let packed = T.pack str in
    -- T.unpack (T.replace "\n\n" "\n;;;\n" packed)

keliParse :: String -> String -> Either KeliError [Raw.Decl] 
keliParse filename input = 
    case parse keliParser filename (preprocess input) of
        Right decls -> Right decls
        Left parseError -> Left (KErrorParseError (errorPos parseError) (errorMessages parseError))
