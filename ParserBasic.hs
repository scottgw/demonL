{-# LANGUAGE FlexibleContexts #-}

module ParserBasic (comma, reserved, identifier, squares, braces, reservedOp, parens, dot, integer, colon, whiteSpace) where

import Control.Monad.Identity

import Text.Parsec
import Text.Parsec.ByteString
import qualified Text.Parsec.Token as P

import Data.ByteString (ByteString)

lexeme :: P.GenTokenParser ByteString () Identity
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
                      ,["Result"]
                      ,["ensure","require","invariant"]
                      ,["Int","Double","Bool"]
                      ],
           P.caseSensitive = True
         }

resOps = concat [["*","+"]
                ,["<=","=", "/="]
                ,["<",">"]
                ,["\"[","]\""]
                ,["{","}",":","."]
                ,["@"]
                ,["not", "and", "and then", "or", "or else", "implies"]
                ]

type BSParser a = ParsecT ByteString () Identity a

-- whiteOut p = whiteSpace *> p

-- whiteSpace = skipMany1 (satisfy isSpace)

-- identifier :: Stream s m Char => ParsecT s u m String
identifier = P.identifier lexeme

-- integer :: Stream s m Char => ParsecT s u m Integer
integer = P.integer lexeme

-- colon :: Parser ()
colon = reservedOp ":"

-- comma :: Stream s m Char => ParsecT s u m String
comma = P.comma lexeme

-- dot :: Stream s m Char => ParsecT s u m String
-- dot :: Parser Char
dot = P.dot lexeme

-- float :: Stream s m Char => ParsecT s u m Double
float = P.float lexeme

-- reserved :: Stream s m Char => String -> ParsecT s u m ()
reserved = P.reserved lexeme

-- reservedOp :: Stream s m Char => String -> ParsecT s u m ()
reservedOp = P.reservedOp lexeme

-- parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = P.parens lexeme

-- braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = P.braces lexeme

-- squares :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
squares = P.squares lexeme

-- whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = P.whiteSpace lexeme