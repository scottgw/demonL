module Types where

import Text.Parsec
import Text.Parsec.ByteString

import ParserBasic

data Type = StructType String [Type]
          | IntType
          | DoubleType
          | VoidType
          | NoType
          | BoolType deriving (Eq, Ord)

isBasic :: Type -> Bool
isBasic IntType    = True
isBasic DoubleType = True
isBasic BoolType   = True
isBasic _          = False

instance Show Type where
    show IntType       = "INTEGER"
    show DoubleType    = "REAL"
    show NoType        = "notype"
    show VoidType      = "NONE"
    show BoolType      = "BOOLEAN"
    show (StructType s []) = s
    show (StructType s gs) = s ++ show gs

typeP :: Parser Type
typeP = intType <|> boolType <|> doubleType <|> classType

intType :: Parser Type
intType = reserved "INTEGER" >> return IntType

doubleType :: Parser Type
doubleType = reserved "REAL" >> return DoubleType

boolType :: Parser Type
boolType = reserved "BOOLEAN" >> return BoolType

classType :: Parser Type
classType = do
  i  <- identifier
  gs <- option [] (squares (sepBy typeP comma))
  return (StructType i gs)
