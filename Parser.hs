{-# LANGUAGE FlexibleContexts #-}
module Parser (domain, serialGoal) where

import Control.Applicative hiding ((<|>), optional, many)

import Data.Map as M

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.ByteString

import AST
import Goal
import ParserBasic
import Types

struct :: Parser Struct
struct = reserved "type" *> 
         (Struct <$> identifier <*> braces (many1 decl))

expr :: Parser Expr
expr = buildExpressionParser table factor

table :: Stream s m Char => OperatorTable s u m Expr
table = 
    [[prefix "old" (UnOpExpr Old)]
    ,[prefix "not" (UnOpExpr Not)
     ,prefix "-"   (UnOpExpr Neg)
     ]
    ,[binary "*"  (BinOpExpr Mul) AssocLeft
     ,binary "/"  (BinOpExpr Div) AssocLeft]
    ,[binary "+"  (BinOpExpr Add) AssocLeft
     ,binary "-"  (BinOpExpr Sub) AssocLeft]
    ,[binary "<=" (BinOpExpr (RelOp Lte NoType)) AssocLeft]
    ,[binary "<"  (BinOpExpr (RelOp Lt  NoType)) AssocLeft]
    ,[binary "="  (BinOpExpr (RelOp Eq  NoType)) AssocLeft]
    ,[binary "/=" (BinOpExpr (RelOp Neq NoType)) AssocLeft]
    ,[binary ">"  (BinOpExpr (RelOp Gt  NoType)) AssocLeft]
    ,[binary ">=" (BinOpExpr (RelOp Gte NoType)) AssocLeft]
    ,[
      binary "and then"  (BinOpExpr Or)   AssocLeft             
     ,binary "and"  (BinOpExpr And)  AssocLeft
     ,binary "or else"  (BinOpExpr Or)   AssocLeft
     ,binary "or"  (BinOpExpr Or)   AssocLeft
     ,binary "implies"  (BinOpExpr Implies)   AssocLeft
     ]
    ]

prefix :: Stream s m Char => 
          String -> (Expr -> Expr) -> Operator s u m Expr
prefix name fun = 
    Prefix $ do
      reservedOp name
      return fun

binary :: Stream s m Char => 
          String -> (Expr -> Expr -> Expr) -> Assoc -> Operator s u m Expr
binary name fun = 
    Infix $ do
      reservedOp name
      return fun

factor = try lookupP <|> try access <|> factor'

factor' :: Parser Expr
factor' = 
      intLit
  <|> boolLit
  <|> void
  <|> try call
  <|> resultVar
  <|> var
  <|> parens expr

access = go =<< (accs =<< factor')
  where 
    accs e = Access e <$> (dot *> identifier)
    go e = try (go =<< accs e) <|> pure e

void = reserved "Void" >> return LitVoid
lookupP = BinOpExpr ArrayIndex <$> factor' <*> squares expr
argsP = parens (expr `sepBy` comma)

resultVar = reserved "Result" *> pure ResultVar
var = Var <$> identifier
call = Call <$> identifier <*> argsP

intLit = fmap (LitInt . fromIntegral) integer
boolLit = (reservedOp "True" >> return (LitBool True)) <|>
          (reservedOp "False" >> return (LitBool False))

-- Procedure bodies
clause = Clause <$> identifier <*> (colon *> expr)
requires = reserved "require" >> many clause
ensures = reserved "ensure" >> many clause

procedureP = 
  Procedure <$> identifier 
            <*> argumentList 
            <*> option NoType (colon >> typeP)
            <*> requires 
            <*> ensures

-- Declaration parsers
decl = Decl <$> identifier <*> (colon *> typeP)
argumentList = parens (decl `sepBy` comma)

-- Domain description
emptyDomain = Domain [] []
addStruct (Domain procs structs) s = Domain procs (s:structs)
addProc (Domain procs structs) p = Domain (p:procs) structs

domain = foldl (\d -> either (addStruct d) (addProc d)) emptyDomain <$> eithers
  where 
    strct = Left <$> struct
    prcs = Right <$> procedureP 
    eithers = many (strct <|> prcs)

assign = Assignment <$> identifier <*> braces nameMap 
  where
    nameMap  = M.fromList <$> many nameExpr
    nameExpr = (,) <$> identifier <*> (reservedOp "=" *> expr)

serialGoal = SerialGoal <$> many decl 
                        <*> many assign 
                        <*> (reservedOp "->" *> expr)