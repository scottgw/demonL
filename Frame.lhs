\begin{code}
module Frame (actionFrame) where

import Data.List
import qualified Data.Map as M

import Math.SMT.Yices.Syntax

import AST hiding (Expr (..))
import Types
import TypeCheck
import Yices

\end{code}

The action frame takes the procedure and generates
an exclusionary predicate.
This exclusionary predicate is run over all references and
will determine if they need to be held immutable for
the execution of this procedure.

\begin{code}
actionFrame :: [Struct] -> ProcedureT -> ExpY
actionFrame types proc = 
    let 
        typeFrames = M.foldrWithKey unmodType (LitB True) (unmodifiedMap types proc)
        orArgs = OR (objArgs $ prcdArgs proc)
        lambda = LAMBDA [("obj", allRefType), idxDecl] (AND [orArgs, typeFrames])
    in APP (VarE "all-frames") [lambda, preIdx]
\end{code}

Auxillary function which constructs a map of which attributes are left
unmodified.
These will become equality constraints in the frame condition.
\begin{code}
unmodifiedMap types proc = 
 let 
     insertStruct (Struct n attrs) = M.insert n (declsToMap attrs)
     typesToMap = foldr insertStruct M.empty types

     removeModified sName attr = M.adjust (M.delete attr) sName
     updateModified modMap e attr = 
         case texprType e of
           StructType sName _ -> removeModified sName attr modMap
           _  -> modMap

     go (LitInt _) modMap = modMap
     go (Var _ _) modMap = modMap
     go (Access e attr _) modMap = updateModified modMap e attr
     go (UnOpExpr _ e _) modMap = go e modMap
     go (BinOpExpr _ e1 e2 _) modMap = go e2 (go e1 modMap)
     go e _ = error $ show e
 in foldr go typesToMap (clauseExprs $ prcdEns proc) 
\end{code}

\begin{code}
unmodEqs typ = AND . map (unmodEq typ)
                       
unmodEq typ attr = 
    APP (VarE attr) [allUnwrap typ objY, preIdx] := 
        APP (VarE attr) [allUnwrap typ objY, postIdx]

unmodType typ unmodAttrs partFrame = 
    IF (APP (VarE $ allWrapStr typ ++ "?") [objY])
           (unmodEqs typ (M.keys unmodAttrs))
           partFrame


objArgs = map objIsArg
objIsArg (Decl n (StructType t _)) = objY := allWrap t (VarE n)
objIsArg _ = LitB False
\end{code}