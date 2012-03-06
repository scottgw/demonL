module Language.DemonL.Types where

import Text.Parsec
import Text.Parsec.ByteString

import Language.DemonL.ParserBasic

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
    show IntType       = "Int"
    show DoubleType    = "Double"
    show NoType        = "notype"
    show VoidType      = "NONE"
    show BoolType      = "Bool"
    show (StructType s []) = s
    show (StructType s gs) = s ++ show gs

typeP :: Parser Type
typeP = intType <|> boolType <|> doubleType <|> classType

intType :: Parser Type
intType = reserved "Int" >> return IntType

doubleType :: Parser Type
doubleType = reserved "Double" >> return DoubleType

boolType :: Parser Type
boolType = reserved "Bool" >> return BoolType

classType :: Parser Type
classType = do
  i  <- identifier
  gs <- option [] (squares (sepBy typeP comma))
  return (StructType i gs)
