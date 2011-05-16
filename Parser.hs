{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative hiding ((<|>), optional, many)

import Data.List
import qualified Data.Map as Map
import Data.Map (Map)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.ByteString

import ParserPosition
import ParserBasic
import Types

data StructType = StructType String [Decl] deriving Show

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Or
           | And
           | Implies
           | RelOp ROp Type
           | SymbolOp String
             deriving (Show, Eq)

data ROp = Lte
         | Lt 
         | Eq 
         | Neq
         | Gt 
         | Gte
           deriving (Show, Eq)

data UnOp = Not
          | Neg
          | Old
            deriving (Show, Eq)

data Expr =
    Call String [Expr]
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  | Access Expr String
  | Var String
  | ResultVar
  | Cast Type Expr
  | LitString String
  | LitChar Char
  | LitInt Int
  | LitBool Bool
  | LitVoid
  | LitDouble Double deriving Eq

instance Show Expr where
    show (Call s args) 
        = s ++ "(" ++ intercalate "," (map show args) ++ ")"
    show (BinOpExpr op e1 e2) 
        = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (UnOpExpr op e) = show op ++ " " ++ show e
    show (Access e f) = show e ++ "." ++ f
    show (Var s) = s
    show ResultVar  = "Result"
    show (Cast t e)    = "{" ++ show t ++ "}" ++ show e
    show (LitString s) = "\"" ++ s ++ "\""
    show (LitChar c) = "'" ++ [c] ++ "'"
    show (LitInt i)  = show i
    show (LitBool b) = show b
    show (LitDouble d) = show d
    show LitVoid = "Void"



struct :: Parser StructType
struct = reserved "type" *> 
         (StructType <$> identifier <*> braces (many1 decl))

expr :: Parser Expr
expr = buildExpressionParser table factor

table :: Stream s m Char => OperatorTable s u m Expr
table = 
    [
     [prefix "not" (UnOpExpr Not)
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
--    ,[otherOperator]
--    ,[accessP]
    ,[prefix "old" (UnOpExpr Old)]
    ]

accessP :: Stream s m Char => Operator s u m Expr
accessP = Postfix $ flip Access <$> (identifier <* dot)

symbols = "*/-+\\|="

symbolsOp :: Stream s m Char => ParsecT s u m String
symbolsOp = many1 (oneOf symbols)

otherOpP :: Stream s m Char => ParsecT s u m String
otherOpP = do
  o <- symbolsOp
  spaces
  return o

otherOperator :: Stream s m Char => Operator s u m Expr
otherOperator = do
  Infix (do
            op <- otherOpP
            return (BinOpExpr (SymbolOp op))
        ) AssocLeft

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

stringLit :: Parser Expr
stringLit = LitString `fmap` anyString

void :: Parser Expr
void = reserved "Void" >> return LitVoid

lookupP :: Parser Expr
lookupP = BinOpExpr (SymbolOp "[]") <$> factor' <*> squares expr

argsP = parens (expr `sepBy` comma)

var = Var <$> identifier

call :: Parser Expr
call = Call <$> identifier <*> argsP

resultVar :: Parser Expr
resultVar = reserved "Result" *> pure ResultVar

intLit :: Parser Expr
intLit = fmap (LitInt . fromIntegral) integer

doubleLit :: Parser Expr
doubleLit = fmap LitDouble float

boolLit :: Parser Expr
boolLit = (reservedOp "True" >> return (LitBool True)) <|>
          (reservedOp "False" >> return (LitBool False))
          
          
-- Procedure bodies


data Clause a = Clause 
    { clauseName :: String
    , clauseExpr :: a
    } deriving Show

data Procedure exp = 
    Procedure
    { 
      prcdName   :: String,
      prcdArgs   :: [Decl],
      prcdResult :: Type,
      prcdReq    :: [Clause exp],
      prcdEns    :: [Clause exp]
    } deriving Show

clause :: Parser (Clause Expr)
clause = do 
  tag <- identifier
  reservedOp ":"
  Clause tag `fmap` expr

requires :: Parser [Clause Expr]
requires = reserved "require" >> many clause

ensures :: Parser [Clause Expr]
ensures = reserved "ensure" >> many clause


procedureP = 
  Procedure <$> identifier 
            <*> argumentList 
            <*> option NoType (colon >> typeP)
            <*> requires 
            <*> ensures

-- Declaration parsers

data Decl = Decl 
    { declName :: String,
      declType :: Type
    }

instance Show Decl where
    show (Decl name typ) = name ++ ": " ++ show typ

insertDecl :: Decl -> Map String Type -> Map String Type
insertDecl (Decl s t) = Map.insert s t

declsToMap :: [Decl] -> Map String Type
declsToMap = foldr insertDecl Map.empty


declEq :: Parser Decl
declEq = do
  d <- decl
  optional (reservedOp "=" >> expr)
  return d

decl :: Parser Decl
decl = do
  name <- identifier <?> "Declaration identifier"
  decl' name

decl' :: String -> Parser Decl
decl' varName = do
  reservedOp ":"     <?> "Declaration ':'"
  typeName <- typeP  <?> "Declaration type"
  return $ Decl varName typeName

argumentList :: Parser [Decl]
argumentList = parens (decl `sepBy` comma)

-- Domain description
 
data Domain = 
  Domain  
  {
    domProcs :: [Procedure Expr],
    domStructs :: [StructType]
  } deriving Show

emptyDomain = Domain [] []
addStruct (Domain procs structs) s = Domain procs (s:structs)
addProc (Domain procs structs) p = Domain (p:procs) structs

domain = foldl (\d -> either (addStruct d) (addProc d)) emptyDomain <$> eithers
  where 
    strct = Left <$> struct
    prcs = Right <$> procedureP 
    eithers = many (strct <|> prcs)

parseExpr = parse expr ""
parseDomain = parse domain ""