module Language.DemonL.PrettyPrint where

import Data.List

import Text.PrettyPrint

import Language.DemonL.AST
import qualified Language.DemonL.TypeCheck as DT
import Language.DemonL.Types

vcatMap f = vsep . intersperse (text "") . map f
argsMap f = parens . hcat . punctuate comma . map f
tab = nest 2
emptyLine = text ""
vsep = foldr ($+$) empty

domainDoc :: (e -> Doc) -> Domain e -> Doc
domainDoc exprDoc (Domain structs procs funcs) = 
  vsep [ vcatMap structDoc structs
       , vcatMap (procDoc exprDoc) procs
       , vcatMap (procDoc exprDoc) funcs
       ]
  
structDoc (Struct name decls) =
  text "type" <+> text name 
    $+$ braces (tab $ vcatMap declDoc decls)
    
declDoc (Decl name ttype) =
  text name <> colon <+> typeDoc ttype

typeDoc IntType = text "Int"
typeDoc DoubleType = text "Double"
typeDoc BoolType = text "Bool"
typeDoc (StructType name []) = text name
typeDoc t = error $ "typeDoc: " ++ show t

procDoc exprDoc (Procedure name args result req ens) = 
  let argsDoc = argsMap declDoc args
      resultDoc = case result of
        NoType -> empty
        t -> colon <+> typeDoc t
      clausesDoc = tab . vcatMap (clauseDoc exprDoc)
      reqDoc = text "require" $?$ clausesDoc req
      ensDoc = text "ensure" $?$ clausesDoc ens
  in text name <+> argsDoc <> resultDoc
       $+$ tab reqDoc
       $+$ tab ensDoc

clauseDoc exprDoc (Clause name expr) = 
  text name <> colon <+> exprDoc expr
  
untypeExprDoc = go
  where
    go (Call name args@[e]) = 
      let opMb = case name of
            "BOOLEAN_negated" -> Just Not
            "INTEGER_32_opposite" -> Just Neg
            _ -> Nothing
      in case opMb of
        Just op -> go (UnOpExpr op e)
        Nothing -> text name <+> argsMap go args
    go (Call name args@[e1,e2]) = 
      let opMb = case name of
            "INTEGER_32_minus" -> Just Sub
            "INTEGER_32_plus" -> Just Add
            "INTEGER_32_is_greater" -> Just (RelOp Gt)
            _ -> Nothing
      in case opMb of
        Just op -> go (BinOpExpr op e1 e2)
        Nothing -> text name <+> argsMap go args
    go (Call name args) = text name <+> argsMap go args
    go (Var name) = text name
    go (BinOpExpr op e1 e2) = parens (go e1 <+> text (show op) <+> go e2)
    go (UnOpExpr op e) = parens (text (show op) <+> go e)
    go (ForAll ds e) = 
      parens (text "forall" <+> argsMap declDoc ds <+> go e)
    go (LitInt i) = integer i
    go (LitBool b) = bool b
    go LitNull = text "null"
    go ResultVar = text "Result"
    go e = error $ "untypeExprDoc: " ++ show e

typeExprDoc = go
  where
    go (DT.Call name args@[e] t) = 
      let opMb = case name of
            "BOOLEAN_negated" -> Just Not
            "INTEGER_32_opposite" -> Just Neg
            _ -> Nothing
      in case opMb of
        Just op -> go (DT.UnOpExpr op e t)
        Nothing -> text name <+> argsMap go args
    go (DT.Call name args@[e1,e2] t) = 
      let opMb = case name of
            "INTEGER_32_minus" -> Just DT.Sub
            "INTEGER_32_plus" -> Just DT.Add
            "INTEGER_32_is_greater" -> Just (DT.RelOp Gt IntType)
            _ -> Nothing
      in case opMb of
        Just op -> go (DT.BinOpExpr op e1 e2 t)
        Nothing -> text name <+> argsMap go args
    go (DT.Call name args _t) = text name <+> argsMap go args
    go (DT.Var name _t) = text name
    go (DT.BinOpExpr op e1 e2 _t) = parens (go e1 <+> text (show op) <+> go e2)
    go (DT.UnOpExpr op e _t) = parens (text (show op) <+> go e)
    go (DT.ForAll ds e) = 
      parens (text "forall" <+> argsMap declDoc ds <+> go e)
    go (DT.LitInt i) = integer i
    go (DT.LitBool b) = bool b
    go (DT.LitNull _t) = text "null"
    go (DT.ResultVar _t) = text "Result"
    go e = error $ "untypeExprDoc: " ++ show e


bool True = text "True"
bool False = text "False"

d1 $?$ d2
  | isEmpty d2 = empty
  | otherwise  = d1 $+$ d2
