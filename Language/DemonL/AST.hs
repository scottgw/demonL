module Language.DemonL.AST where

import Data.List (intercalate, find)
import qualified Data.Map as M

import Language.DemonL.Types

data Decl = Decl 
    { declName :: String,
      declType :: Type
    } deriving (Eq, Ord)

declsToMap = foldr ( \ d -> M.insert (declName d) (declType d)) M.empty

instance Show Decl where
    show (Decl name typ) = name ++ ": " ++ show typ


data Clause a = Clause 
    { clauseName :: String
    , clauseExpr :: a
    } deriving Show

type ProcedureU = Procedure Expr

data Procedure exp = 
    Procedure
    { 
      prcdName   :: String,
      prcdArgs   :: [Decl],
      prcdResult :: Type,
      prcdReq    :: [Clause exp],
      prcdEns    :: [Clause exp]
    } deriving Show


type DomainU = Domain Expr

data Domain e = 
  Domain  
  { domStructs :: [Struct]
  , domProcs :: [Procedure e]
  , domFuncs :: [Procedure e]
  } deriving Show


findProc dom name = find ((== name) . prcdName) (domProcs dom)
findProcUnsafe dom name = let Just p = findProc dom name in p

data Struct = 
  Struct 
  { structName :: String 
  , structDecls :: [Decl] 
  } deriving Show

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Or
           | And
           | Implies
           | ArrayIndex
           | RelOp ROp
             deriving (Ord, Eq)

data ROp = Lte
         | Lt 
         | Eq 
         | Neq
         | Gt 
         | Gte
           deriving (Ord, Eq)

data UnOp = Not
          | Neg
          | Old
            deriving (Ord, Eq)

data Expr = 
  Call String [Expr]
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  | Access Expr String
  | Var String
  | ResultVar
  | Cast Type Expr
  | ForAll [Decl] Expr
  | LitInt Integer
  | LitBool Bool
  | LitDouble Double
  | LitNull
  deriving (Ord, Eq)

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Or  = "or"
  show And = "and"
  show Implies = "implies"
  show (RelOp op) = show op
  
instance Show ROp where
  show Lte = "<="
  show Lt  = "<"
  show Eq  = "="
  show Neq = "/="
  show Gt  = ">"
  show Gte = ">="

instance Show UnOp where
  show Old = "old"
  show Neg = "-"
  show Not = "not"

instance Show Expr where
    show (Call s args) 
        = s ++ "(" ++ intercalate "," (map show args) ++ ")"
    show (BinOpExpr op e1 e2) 
        = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
    show (UnOpExpr op e) = show op ++ " (" ++ show e ++ ")"
    show (Access e f)  = show e ++ "." ++ f
    show (Var s)       = s
    show (ForAll decls e) = 
      "forall: " ++ intercalate "," (map show decls) ++ " " ++ show e
    show ResultVar     = "Result"
    show (Cast t e)    = "{" ++ show t ++ "}" ++ show e
    show (LitInt i)    = show i
    show (LitBool b)   = show b
    show (LitDouble d) = show d
    show LitNull       = "null"
