module Lexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef :: LanguageDef st
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.opStart         = oneOf "~!@#$%^&*-=+:?/<>\\"
           , Token.opLetter        = oneOf "~!@#$%^&*-=+:?/<>\\"
           , Token.identStart      = letter   <|> char '_' <|> char '#' <|> char '$'
           , Token.identLetter     = alphaNum <|> char '_' <|> char '#' <|> char '$'
           , Token.reservedOpNames = [
                --  "="
                "|"
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
natural    = Token.natural        lexer
semi       = Token.semi           lexer -- parses a semicolon
whiteSpace = Token.whiteSpace     lexer -- parses whitespace
symbol     = Token.symbol         lexer -- custom symbol
dot        = Token.dot            lexer
operator   = Token.operator       lexer
singlelineString = Token.stringLiteral  lexer

