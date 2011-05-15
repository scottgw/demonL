module Types where

import Text.Parsec
import Text.Parsec.ByteString

import ParserBasic

data Type = ClassType ClassName [Type]
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
    show (ClassType s gs) = s ++ show gs


type ClassName = String

classNameType :: Type -> String
classNameType (ClassType cn _) = cn 
classNameType _ = error "Non-class type"

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
  return (ClassType i gs)

detType :: Parser Type
detType = reserved "detachable" >> baseType

attType :: Parser Type
attType = reserved "attached" >> baseType

typeP :: Parser Type
typeP = detType <|> attType <|> baseType

baseType :: Parser Type
baseType = intType <|> boolType <|> doubleType <|> classType