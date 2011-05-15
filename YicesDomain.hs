{-# LANGUAGE OverloadedStrings #-}
module YicesDomain where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.AttoLisp as L
import Parser
import Types

declsY :: [Decl] -> L.Lisp
declsY ds = list (concatMap declY ds)

declY :: Decl -> [L.Lisp]
declY (Decl name typ) = [symbol name, symbol "::", basicTypeY typ]

basicTypeY :: Type -> L.Lisp
basicTypeY IntType = symbol "int"
basicTypeY BoolType = symbol "bool"
basicTypeY DoubleType = symbol "real"
basicTypeY VoidType = symbol "NONE"
basicTypeY (ClassType n _) = symbol n
basicTypeY NoType = error "no type"

-- Lambda conversion

lambdaY :: [Decl] -> L.Lisp -> L.Lisp
lambdaY args expr = list [symbol "lambda", declsY args, expr]

list = L.List

-- Expression conversion
exprY :: Expr -> L.Lisp
exprY (Call name args) = list (symbol name : map exprY args)
exprY (Var v) = symbol v
exprY (BinOpExpr bop e1 e2) = exprY (Call (binLisp bop) [e1, e2])
exprY (LitInt i) = symbol (show i)
exprY (LitBool True) = symbol "true"
exprY (LitBool False) = symbol "false"

binaryLisp = symbol . binLisp

binLisp Add = "+"
binLisp Sub = "-"
binLisp Mul = "*"
binLisp Div = "/"
binLisp Or = "or"
binLisp And = "and"
binLisp Implies = "implies"
binLisp (SymbolOp s) = s
binLisp (RelOp op _) = relLisp op

relLisp Lte = "<="
relLisp Lt  = "<"
relLisp Eq  = "="
relLisp Neq = "/="
relLisp Gt  = ">"
relLisp Gte = ">="

andY es = list $ symbol "and" : es

b2t :: B.ByteString -> T.Text
b2t = T.pack . B.unpack

symbol :: String -> L.Lisp
symbol = L.Symbol . T.pack

clauseY  = exprY . clauseExpr
clausesY = map clauseY


data Define = 
  Define 
  {
    defName :: String,
    defType :: YicesType,
    defExpr :: Maybe L.Lisp
  }

actionExpr (Procedure {prcdArgs = args, prcdReq = req, prcdEns = ens}) =
  let
    pres  = clausesY req
    posts = clausesY ens
    ands  = andY $ pres ++ posts
  in lambdaY args ands
   
  
data YicesType = BasicType Type
               | FuncType [YicesType] YicesType

typeY (BasicType t) = basicTypeY t
typeY (FuncType ts r) = list $ symbol "->" : map typeY (ts ++ [r])

indexType = BasicType IntType

actionType = FuncType [indexType] (BasicType BoolType)

procYicesType :: Procedure Expr -> YicesType
procYicesType (Procedure {prcdArgs = args, prcdResult = resultType}) = 
  let ytypes = map (BasicType . declType) args
  in  case resultType of
    NoType -> FuncType ytypes actionType
    _ -> error "only works on proper procedures, without a result"

defineY (Define {defName = name, defType = typ, defExpr = exprM}) = 
  let noExpr = [symbol "define", symbol name, symbol "::", typeY typ]
  in  list $ noExpr ++ maybe [] (:[]) exprM

procConv :: Procedure Expr -> L.Lisp
procConv proc@(Procedure {prcdName = name, prcdReq = req, prcdEns = ens}) = 
  let
    pres  = clausesY req
    posts = clausesY ens
    ands  = andY $ pres ++ posts
  in defineY $ Define name (procYicesType proc) (Just $ actionExpr proc)

-- read and convert the test-domain to yices format
testCase = do
  str <- B.readFile "test.dmn"
  let Right doms = parseDomain str
  mapM_ (putStrLn . show . procConv) doms