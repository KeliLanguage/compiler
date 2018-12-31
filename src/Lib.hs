module Lib where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


data KeliExpr 
    = KeliNumber Float
    | KeliString String
    | KeliArray  [KeliExpr]
    | KeliTuple  [KeliExpr]
    | KeliId     String
    | KeliFuncCall {
        funcCallParams     :: [KeliExpr],
        funcCallSignaturee :: [String]
    }
    | KeliLambda {
        lambdaParams :: [String],
        lambdaBody   :: KeliExpr
    }


data BExpr 
    = BoolConst Bool
    | Not BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr
    deriving (Show)

data BBinOp 
    = And 
    | Or 
    deriving (Show)

data RBinOp 
    = Greater 
    | Less 
    deriving (Show)

data AExpr 
    = Var String
    | IntConst Integer
    | Neg AExpr
    | ABinary ABinOp AExpr AExpr
    deriving (Show)

data ABinOp 
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Show)

data Stmt 
    = Seq [Stmt]
    | Assign String AExpr
    | If BExpr Stmt Stmt
    | While BExpr Stmt
    | Skip
    deriving (Show)


languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =  parens statement <|> sequenceOfStmt

sequenceOfStmt = do
    list <- (sepBy1 statement' semi)
    return (if length list == 1 then head list else Seq list)

statement' :: Parser Stmt
statement' 
    = ifStmt
   <|> whileStmt
   <|> skipStmt
   <|> assignStmt

ifStmt :: Parser Stmt
ifStmt 
    =  reserved "if"   >>= \_
    -> bExpression     >>= \cond 
    -> reserved "then" >>= \_
    -> statement       >>= \stmt1
    -> reserved "else" >>= \_
    -> statement       >>= \stmt2
    -> return (If cond stmt1 stmt2)

whileStmt :: Parser Stmt
whileStmt 
    =  reserved "while" >>= \_
    -> bExpression      >>= \cond
    -> statement        >>= \stmt
    -> return (While cond stmt)

skipStmt :: Parser Stmt
skipStmt 
    =  reserved "skip" >>= \_
    -> return Skip

assignStmt :: Parser Stmt
assignStmt 
    =  identifier      >>= \var
    -> reservedOp ":=" >>= \_
    -> aExpression     >>= \expr
    -> return (Assign var expr)

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm
    
aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
             ]

aTerm 
    =  parens aExpression
   <|> liftM Var identifier
   <|> liftM IntConst integer

bTerm
    =   parens bExpression
    <|> (reserved "true"  >> return (BoolConst True))
    <|> (reserved "false" >> return (BoolConst False))
    <|> rExpression


rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)


parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r