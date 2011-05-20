{-# LANGUAGE OverloadedStrings #-}
module YicesDomain (testCase) where

import qualified Data.ByteString.Char8 as B
import Math.SMT.Yices.Syntax

import AST
import Parser
import Types

-- Type conversion

basicTypeY :: Type -> TypY
basicTypeY IntType = VarT "int"
basicTypeY BoolType = VarT "bool"
basicTypeY DoubleType = VarT "real"
basicTypeY VoidType = VarT "NONE"
basicTypeY (StructType n _) = VarT $ structName n
basicTypeY NoType = error "no type"

structName n = n ++ "_ref"

-- Expression conversion
exprY :: ExpY -> Expr -> ExpY
exprY i (Call name args) = APP (APP (VarE name) (map (exprY i) args)) [i]
exprY i (BinOpExpr bop e1 e2) = binYices bop (exprY i e1) (exprY i e2)
exprY i (UnOpExpr uop e) = unaryYices uop i e
exprY i (Access e f) = exprY i (Call f [e])
exprY _ (Var v) = VarE v
exprY _ (LitInt int) = LitI (fromIntegral int)
exprY _ (LitBool b) = LitB b
exprY _ (LitDouble d) = LitR (toRational d)

unaryYices Not i e = NOT (exprY i e)
unaryYices Neg i e = LitI 0 :-: exprY i e
unaryYices Old _ e = exprY preIdx e 

binYices Add = (:+:)
binYices Sub = (:-:)
binYices Mul = (:*:)
binYices Div = (:/:)
binYices Implies = (:=>)
binYices Or = \ x y -> OR [x, y]
binYices And = \ x y -> AND [x, y]
binYices ArrayIndex = \ x y -> APP x [y]
binYices (RelOp op _) = relYices op

relYices Lte = (:<=)
relYices Lt  = (:<)
relYices Eq  = (:=)
relYices Neq = (:/=)
relYices Gt  = (:>)
relYices Gte = (:>=)

clauseExprs = map clauseExpr

-- Action creation

declsToArgsY :: [Decl] -> [(String, TypY)]
declsToArgsY = concatMap declY
  where declY (Decl name typ) = [(name, basicTypeY typ)]

idxStr = "idx"
preIdx = VarE idxStr
postIdx = incr preIdx
incr = (:+: LitI 1)
indexType = basicTypeY IntType

actionType = ARR [indexType, basicTypeY BoolType]

actionBody :: [Expr] -> [Expr] -> ExpY
actionBody pres posts = 
  actionBodyLambda (AND $ map (exprY preIdx) pres ++ map (exprY postIdx) posts)
  
actionBodyLambda = LAMBDA [(idxStr, indexType)]
        
actionExpr :: Procedure Expr -> ExpY
actionExpr (Procedure {prcdArgs = args, prcdReq = req, prcdEns = ens}) =
  let
    pres  = clauseExprs req
    posts = clauseExprs ens
    body  = actionBody pres posts
  in LAMBDA (declsToArgsY args) body

attrType :: String -> Type -> TypY
attrType structType resultType = 
    ARR  [basicTypeY (StructType structType []),
          ARR [indexType, basicTypeY resultType]]

procYicesType :: Procedure Expr -> TypY
procYicesType (Procedure {prcdArgs = args, prcdResult = resultType}) = 
  let ytypes = map (basicTypeY . declType) args
  in  
   case resultType of
     NoType -> ARR (ytypes ++ [actionType])
     _ -> error "only works on proper procedures, without a result"

procConvY :: Procedure Expr -> CmdY
procConvY proc =
  DEFINE (prcdName proc, procYicesType proc) (Just $ actionExpr proc)

maxObjs :: Int
maxObjs = 10

structConvY (Struct name _) = DEFTYP (structName name) (Just $ SCALAR objs)
    where idxRefObj nm i = nm ++ "_obj" ++ show i
          objs = map (idxRefObj name) [1 .. maxObjs]

attrConvY (Struct name decls) = map (declToFunction name) decls

declToFunction :: String -> Decl -> CmdY
declToFunction typeName (Decl name resultType) =
  DEFINE (attrFuncName, attrType typeName resultType) Nothing
  where attrFuncName = typeName ++ "_" ++ name

procDom :: Domain -> [CmdY]
procDom (Domain procs types) = 
    let actions  = map procConvY procs 
        attrs    = concatMap attrConvY types
        refTypes = map structConvY types
        eqs      = map structEquals types
    in concat [refTypes
              ,attrs
              ,eqs
              ,actions
              ]

-- Frame condition
structEquals (Struct name decls) = 
  let
    obj = Var "obj"
    accessObj dn = Access obj dn
    eqExpr (Decl dn _) = BinOpExpr (RelOp Eq NoType)
                                   (accessObj dn)
                                   (UnOpExpr Old $ accessObj dn)
    eqAttrs = map eqExpr decls
    lamExpr = actionBodyLambda $ AND $ map (exprY postIdx) eqAttrs
  in DEFINE (name ++ "_eq", actionType) (Just lamExpr)
        

-- read and convert the test-domain to yices format
testCase = do
  str <- B.readFile "test.dmn"
  let domE = parseDomain str
  print domE
  case domE of
    Right dom -> mapM_ (putStrLn . show) (procDom dom)
    Left e -> print e