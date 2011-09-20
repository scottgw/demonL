{-# LANGUAGE FlexibleContexts #-}
module Parser (domain, serialGoal, expr) where

import Control.Applicative hiding ((<|>), optional, many)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.ByteString

import AST
import Goal
import ParserBasic
import Types

expr :: Parser Expr
expr = buildExpressionParser table factor

-- table :: Stream s m Char => OperatorTable s u m Expr
table = 
    [[binary "."   dotBinary AssocLeft]
    ,[prefix "old" (UnOpExpr Old)]
    ,[prefix "not" (UnOpExpr Not)
     ,prefix "-"   (UnOpExpr Neg)
     ]
    ,[binary "*"  (BinOpExpr Mul) AssocLeft
     ,binary "/"  (BinOpExpr Div) AssocLeft]
    ,[binary "+"  (BinOpExpr Add) AssocLeft
     ,binary "-"  (BinOpExpr Sub) AssocLeft]
    ,[binary "<=" (BinOpExpr (RelOp Lte NoType)) AssocLeft
     ,binary "<"  (BinOpExpr (RelOp Lt  NoType)) AssocLeft
     ,binary "="  (BinOpExpr (RelOp Eq  NoType)) AssocLeft
     ,binary "/=" (BinOpExpr (RelOp Neq NoType)) AssocLeft
     ,binary ">"  (BinOpExpr (RelOp Gt  NoType)) AssocLeft
     ,binary ">=" (BinOpExpr (RelOp Gte NoType)) AssocLeft
     ]
    ,[binary "and"  (BinOpExpr And)  AssocLeft
     ,binary "or"  (BinOpExpr Or)   AssocLeft
     ,binary "implies"  (BinOpExpr Implies)   AssocLeft
     ]
    ]

dotBinary e (Var i) = Access e i

prefix name fun = 
    Prefix $ do
      reservedOp name
      return fun

-- binary :: Stream s m Char => 
--           String -> (Expr -> Expr -> Expr) -> Assoc -> Operator s u m Expr
binary name fun = 
    Infix $ do
      reservedOp name
      return fun

factor :: Parser Expr
factor = 
      intLit
  <|> nullLit
  <|> boolLit
  <|> resultVar
  <|> try call
  <|> var  
  <|> parens expr

nullLit = reserved "null" *> pure LitNull
argsP = parens (expr `sepBy` comma)

resultVar = reserved "Result" *> pure ResultVar
var = Var <$> (identifier <* notFollowedBy (char '('))
call = Call <$> identifier <*> argsP

intLit = fmap (LitInt . fromIntegral) integer
boolLit = (reservedOp "True" >> return (LitBool True)) <|>
          (reservedOp "False" >> return (LitBool False))

-- Procedure bodies
clause = try $ Clause <$> identifier <*> (colon *> expr)
requires = reserved "require" >> many clause
ensures = reserved "ensure" >> many clause

procedureP = 
  Procedure <$> identifier 
            <*> argumentList 
            <*> option NoType (colon >> typeP)
            <*> option [] requires 
            <*> option [] ensures

-- Declaration parsers
decl = Decl <$> identifier <*> (colon *> typeP)
argumentList = parens (decl `sepBy` comma)

-- Domain description
struct :: Parser Struct
struct = reserved "type" *> 
         (Struct <$> identifier <*> braces (many decl))

emptyDom = Domain [] [] []
addStruct (Domain ss ps fs) s = Domain (s:ss) ps fs
addProc   (Domain ss ps fs) p 
  | prcdResult p == NoType  = Domain ss (p:ps) fs
  | otherwise               = Domain ss ps (p:fs)
                           
domain = 
  let
    upd d   = addStruct d <$> struct <|> addProc d <$> procedureP
    domP d  = (domP =<< upd d) <|> pure d
  in optional whiteSpace *> domP emptyDom


-- Goal description
serialGoal = 
  optional whiteSpace *> (SerialGoal <$> many (try decl)
                          <*> many expr
                          <*> (reservedOp "@" *> integer)
                          <*> (reservedOp "->" *> expr))