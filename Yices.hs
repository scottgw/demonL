module Yices where

import Math.SMT.Yices.Syntax

import Types
import AST hiding (Expr (..))
import TypeCheck

-- Type conversion
basicTypeY :: Type -> TypY
basicTypeY IntType = intTypeY
basicTypeY BoolType = boolTypeY
basicTypeY DoubleType = VarT "real"
basicTypeY VoidType = VarT "NONE"
basicTypeY (StructType n _) = VarT $ structStr n
basicTypeY NoType = error "no type"

intTypeY = VarT "int"
boolTypeY = VarT "bool"

structStr n = n ++ "_ref"

-- Expression conversion
exprY :: ExpY -> TExpr -> ExpY
exprY i (Call name args t) = APP (VarE name) (map (exprY i) args ++ [i])
exprY i (BinOpExpr bop e1 e2 t) = binYices bop (exprY i e1) (exprY i e2)
exprY i (UnOpExpr uop e t) = unaryYices uop i e
exprY i (Access e f t) = exprY i (Call f [e] t)
exprY _ (Var v t) = VarE v
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


declsToArgsY :: [Decl] -> [(String, TypY)]
declsToArgsY = concatMap declY
  where declY (Decl name typ) = [(name, basicTypeY typ)]

clauseExprs = map clauseExpr

allRefType = VarT allRefStr
allRefStr = "ALL_ref"
allType types = DEFTYP allRefStr (Just $ DATATYPE $ map mkTyCon types)
  where mkTyCon typ = let n = structName typ
                      in (allWrapStr n, [(allUnwrapStr n, VarT (structStr n))])
allUnwrapStr str = structStr str ++ "_unwrap"
allUnwrap str v = APP (VarE $ allUnwrapStr str) [v]
allWrapStr str = structStr str ++ "_wrap"
allWrap str v = APP (VarE $ allWrapStr str) [v]

objY = VarE "obj"

outputFileName fn = fn ++ ".lisp"

showCmds :: [CmdY] -> String
showCmds = unlines . map show

writeYices name cmds = 
  writeFile (outputFileName name) (showCmds cmds) >> return cmds


