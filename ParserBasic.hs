{-# LANGUAGE FlexibleContexts #-}

module ParserBasic where

import Text.Parsec
import Text.Parsec.ByteString
import qualified Text.Parsec.Token as P


lexeme :: Stream s m Char => P.GenTokenParser s u m
lexeme = 
    P.makeTokenParser $ P.LanguageDef
         {
           P.commentStart   = "{-",
           P.commentEnd     = "-}",
           P.commentLine    = "--",
           P.nestedComments = True,
           P.identStart     = letter,
           P.identLetter    = alphaNum <|> oneOf "_'" ,
           P.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~",
           P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~",
           P.reservedOpNames = resOps,
           P.reservedNames = 
               concat [["True","False"]
                      ,["Void"]
                      ,["type"]
                      ,["attached","as"]
                      ,["create"]
                      ,["Result", "Current"]
                      ,["ensure","require","invariant"]
                      ,["INTEGER","REAL","BOOLEAN"]
                      ],
           P.caseSensitive = True
         }

resOps = concat [["*","+"]
                ,["<=","=", "/="]
                ,["<",">"]
                ,["\"[","]\""]
                ,[";","{","}",":","."]
                ,["not", "and", "and then", "or", "or else", "implies"]
                ]

identifier :: Stream s m Char => ParsecT s u m String
identifier = P.identifier lexeme

integer :: Stream s m Char => ParsecT s u m Integer
integer = P.integer lexeme

colon :: Parser ()
colon = reservedOp ":"

comma :: Stream s m Char => ParsecT s u m String
comma = P.comma lexeme

dot :: Stream s m Char => ParsecT s u m String
dot = P.dot lexeme

semicolon :: Parser ()
semicolon = reservedOp ";"

float :: Stream s m Char => ParsecT s u m Double
float = P.float lexeme

reserved :: Stream s m Char => String -> ParsecT s u m ()
reserved = P.reserved lexeme

reservedOp :: Stream s m Char => String -> ParsecT s u m ()
reservedOp = P.reservedOp lexeme

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = P.parens lexeme

angles :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
angles = P.angles lexeme

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = P.braces lexeme

squares :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
squares = P.squares lexeme

stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = P.stringLiteral lexeme

anyString :: Parser String
anyString = blockString <|> stringLiteral

blockString :: Parser String
blockString = reservedOp "\"[" >> manyTill anyChar (reservedOp "]\"")

eol :: Parser ()
eol = semicolon <|> option () (newline >> return ())