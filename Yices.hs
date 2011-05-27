module Yices where

import Math.SMT.Yices.Syntax

import Types
import AST

-- Type conversion
basicTypeY :: Type -> TypY
basicTypeY IntType = intTypeY
basicTypeY BoolType = boolTypeY
basicTypeY DoubleType = VarT "real"
basicTypeY VoidType = VarT "NONE"
basicTypeY (StructType n _) = VarT $ structName n
basicTypeY NoType = error "no type"

intTypeY = VarT "int"
boolTypeY = VarT "bool"

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


idxDecl = (idxStr, indexType)
idxStr = "idx"
preIdx = VarE idxStr
postIdx = incr preIdx
incr = (:+: LitI 1)
indexType = basicTypeY IntType
