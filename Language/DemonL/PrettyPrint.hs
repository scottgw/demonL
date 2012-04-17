module Language.DemonL.PrettyPrint where

import Text.PrettyPrint

import Language.DemonL.AST
import Language.DemonL.Types

vcatMap f = sep . punctuate empty . map f
argsMap f = parens . hcat . punctuate comma . map f
tab = nest 2

domainDoc :: (e -> Doc) -> Domain e -> Doc
domainDoc exprDoc (Domain structs procs funcs) = 
  sep [ vcatMap structDoc structs
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
        t -> colon <> typeDoc t
      clausesDoc = tab . vcatMap (clauseDoc exprDoc)
      reqDoc = text "require" $$ clausesDoc req
      ensDoc = text "ensure" $$ clausesDoc ens
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
    go (LitInt i) = integer i
    go LitNull = text "null"
    go ResultVar = text "Result"
    go e = error $ "untypeExprDoc: " ++ show e