module Language.DemonL.Yices where

import Math.SMT.Yices.Syntax

import Language.DemonL.Types
import Language.DemonL.AST hiding (Expr (..), BinOp (..))
import Language.DemonL.TypeCheck

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

newtype TimeContext = TC ExpY

funcIdxParam = "idx"

timeCtx :: Integer -> TimeContext
timeCtx i = TC $ APP timeState [LitI $ fromIntegral i]

timeState = VarE "query_context"

thisTimeRec = APP timeState [VarE funcIdxParam]

thisTime = TC $ APP timeState [VarE funcIdxParam]
nextTime = TC $ APP timeState [VarE funcIdxParam :+: LitI 1]

updateTimeCtx (TC t) e = t := e

selectFunc :: TimeContext -> String -> ExpY
selectFunc (TC t) = SELECT_R t

-- Expression conversion
exprY :: TimeContext -> TimeContext -> TExpr -> ExpY
exprY pre post (Call name args t) = 
  APP (selectFunc post name) (map (exprY pre post) args)
exprY pre post (BinOpExpr bop e1 e2 t) = 
    binYices bop (exprY pre post e1) (exprY pre post e2)
exprY pre post (UnOpExpr uop e t) = unaryYices uop pre post e
exprY pre post (Access e f t) = exprY pre post (Call f [e] t)
exprY _ _ (Var v t) = VarE v
exprY _ _ (LitInt int) = LitI (fromIntegral int)
exprY _ _ (LitBool b) = LitB b
exprY _ _ (LitDouble d) = LitR (toRational d)
exprY _ _ (LitNull t) = nullValue t
exprY pre post (ForAll ds e) = FORALL (declsToArgsY ds) (exprY pre post e)
exprY _ _ e = error $ "exprY: " ++ show e

replace old new = go
  where
    rep = replace old new
    go e | e == old = new
    go (Call name args t) = Call name (map rep args) t
    go (BinOpExpr bop e1 e2 t) = BinOpExpr bop (rep e1) (rep e2) t
    go (UnOpExpr uop e t) = UnOpExpr uop (rep e) t
    go (Access e f t) = Access (rep e) f t
    go e = e

nullValue (StructType n _) = VarE $ nullStr n
nullStr n = "null_" ++ n

unaryYices Not pre post e = NOT (exprY pre post e)
unaryYices Neg pre post e = LitI 0 :-: exprY pre post e
unaryYices Old pre post e = exprY pre pre e 

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

and' [] = LitB True
and' as = AND as

or' [] = LitB False
or' as = OR as

allRefType = VarT allRefStr
allRefStr = "ALL_ref"
allType types = DEFTYP allRefStr typedef
  where 
    typedef = case types of
                [] -> Nothing
                _  -> Just $ DATATYPE $ map mkTyCon types
    mkTyCon typ = let n = structName typ
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
