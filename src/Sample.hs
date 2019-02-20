module Sample where

import Text.ParserCombinators.Parsec hiding (token)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error 
import Lexer

expr    = buildExpressionParser table term
       <?> "expression"

term    =  parens expr
         <|> natural
         <?> "simple expression"

table   = [ [prefix "-" negate, prefix "+" id ]
            , [postfix "+" (+1)]
            , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
            , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
           ]

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

parseSample str = parse expr "test" str