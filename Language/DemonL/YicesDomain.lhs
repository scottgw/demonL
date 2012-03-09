\documentclass {article}
%include polycode.fmt

\begin{document}

\begin{code}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Language.DemonL.YicesDomain (procDom) where

import Data.List

import Math.SMT.Yices.Syntax

import Language.DemonL.AST hiding (Expr (..))
import Language.DemonL.Types
import Language.DemonL.TypeCheck
import Language.DemonL.Yices
\end{code}

The procedure tags (allowing us to track which actions were used)
and reference type definitions are not dependent on
other declarations in the file, 
so they will be given first.

The reference type declarations also require the generation of the
``top'' type which can be any of the reference types.
For simplicity, we also generate the argument array definitions as well.
\begin{code}
tagsAndTypes types procs = procTagsAndArray ++ refDefines
    where
      procTagsAndArray  =  [procTags procs, tagArray]
      refDefines        =  allType types : argArrayDefines types
\end{code}

The actions that are generated from procedures have to main components:
\begin{itemize}
\item The actions themselves, incorporating the state-change semantics and
  framing rules.
\item The selection of the actions that can be taken at any particular
  time index.
\end{itemize}
\begin{code}
actions procs = map procConvY procs ++ [actionOptions procs]
\end{code}

Finally, the generation of the Yices statements above can be
brought to gether in the he |procDom| function. 
This is the top level conversion from
a domain to a series of Yices commands.

\begin{code}
procDom :: DomainU -> [CmdY]
procDom untypedDom  = 
    let eiDom = runTypeM $ typecheckDomain untypedDom
    in case eiDom of
      Left errStr -> error $ "Error typechecking domain: " ++ errStr
      Right (Domain types procs funcs) ->
         concat  [ tagsAndTypes types procs
                 , [timedContext types funcs]
                 , functionByAssert funcs
                 , actions procs
                 ]
\end{code}

Action Creation

The action type is the result type of any action,
namely a predicate on the state index to a boolean.
This represents that an action can be true in a particular state.
\begin{code}
indexedResult t = ARR [indexType, basicTypeY t]
actionType = indexedResult BoolType
\end{code}
\begin{code}
actionBody :: [ExpY] -> [TExpr] -> [TExpr] -> ExpY
actionBody additional pres posts =
    let 
        ctx0 = LET [(("ctx_0", Nothing), thisTimeRec)]
        mapExpr next = map (exprY thisTime next)
        prePosts = mapExpr thisTime pres ++ map buildPost posts
    in actionBodyLambda $ ctx0 $ and' $ prePosts ++ additional
\end{code}

\begin{code}
actionBodyLambda = LAMBDA [(idxStr, indexType)]
\end{code}

\begin{code}
actionExpr :: ProcedureT -> ExpY
actionExpr proc =
  let
    pres   = clauseExprs (prcdReq proc)
    posts  = clauseExprs (prcdEns proc)
    tag    = APP (VarE "tag_array") [preIdx] := VarE (tagName proc)
    body   = actionBody [tag] pres posts
  in LAMBDA (declsToArgsY (prcdArgs proc)) body
\end{code}

\begin{code}
buildPost :: TExpr -> ExpY
buildPost e =
  let ((i, f), e') = buildQueryEnv 0 e
  in f (e' i)

buildQueryEnv :: Int -> TExpr -> ((Int, ExpY -> ExpY), Int -> ExpY)
buildQueryEnv i = go
  where
    noMod i e = ((i, id), const e)
    
    go (LitNull t)    = noMod i $ nullValue t
    go (LitDouble d)  = noMod i $ LitR (toRational d)
    go (LitBool b)    = noMod i $ LitB b
    go (LitInt int)   = noMod i $ LitI (fromIntegral int)
    go (Var v t)      = noMod i $ VarE v
    go (Access e f t) = buildQueryEnv i (Call f [e] t)
    go (BinOpExpr bop e1 e2 t) =
      let ((q1, ctx1), e1') = buildQueryEnv i e1
          ((q2, ctx2), e2') = buildQueryEnv q1 e2
          ctx' = ctx1 . ctx2
      in ( (q2, ctx'),\x -> binYices bop (e1' x) (e2' x))
    go (UnOpExpr Old e t) = noMod i $ exprY thisTime thisTime e
    go (UnOpExpr uop e t) =
      let ((i', ctx), e') = buildQueryEnv i e          
      in ( (i', ctx), \x -> unary uop (e' x))
    go (Call name args t) =
      let f (queryState, ctx) e = 
            let ((queryState', ctx'), e') = buildQueryEnv queryState e
            in ((queryState', ctx . ctx'), e')

          ((i', ctx'), args') = mapAccumL f (i, id) args
          
          freshI = i' + 1
          
          ctxVar = VarE . ctxName
          ctxName x = "ctx_" ++ show x
          
          exResult x = "ex_" ++ name ++ "_" ++ show x
          
          argsAt as x = map ($ x) as
          
          getFunc x = SELECT_R (ctxVar x)
          updateFunc = UPDATE_F (getFunc freshI name)
                                (argsAt args' i') 
                                (VarE $ exResult freshI)
          updated = UPDATE_R (ctxVar i') name updateFunc
          mkCtx e = 
            EXISTS [(exResult freshI, basicTypeY t)]
                   (LET [((ctxName freshI, Nothing), updated)] e)
                                    
      in ((freshI, ctx' . mkCtx), \x -> APP (getFunc x name) (argsAt args' x))

unary Not = NOT 
unary Neg = (LitI 0 :-:)


exampleExpr = 
  BinOpExpr (RelOp Lt NoType) ex2 (LitInt 6) BoolType
    
ex2 = 
  let g = Call "g" [Var "this" IntType] IntType
  in Call "f" [g, UnOpExpr Old g IntType] IntType

timedContext types funcs =
  let 
    allFuncs :: [(String, TypY)]
    allFuncs = concatMap structFuncs types ++ map funcFuncs funcs
      
    funcFuncs (Procedure name args res _ _) =
      (name, ARR $ map (basicTypeY . declType) args ++ [basicTypeY res])
      
    structFuncs (Struct name decls) = map (declFunc name) decls
    declFunc typeName (Decl name typ) = (name, attrType typeName typ)
    attrType typeName typ = ARR [ basicTypeY (StructType typeName [])
                                , basicTypeY typ]
  in DEFINE ("query_context", ARR [intTypeY, REC allFuncs]) Nothing

  

\end{code}
%
Use a forall expression to denote the behaviour of a function.
This can be used for attributes as well, as they have trivial
pre- and postconditions (true).
\begin{code}
funcAssertion f =   
  let
    declToVar (Decl n t) = Var n t
    argList = map (\ (Decl n t) -> (n, basicTypeY t)) (prcdArgs f)
    newResult = Call (prcdName f) (map declToVar (prcdArgs f)) (prcdResult f)
    resultReplace = replace (ResultVar (prcdResult f)) newResult
    pre  = and' (map (exprY thisTime thisTime . clauseExpr) (prcdReq f))
    post = and' (map (exprY thisTime thisTime . resultReplace . clauseExpr) (prcdEns f))
    body = pre :=> post
  in FORALL ((idxStr, indexType) :  argList) body
\end{code}
%
This then has to be used in an `assert' to ensure the proper behaviour
of the function. This is likely to induce the SMT solver to
give ``unknown'' as an answer, but we can overlook that for now.
\begin{code}
functionByAssert fs = 
  let function f = ASSERT (funcAssertion f)
  in map function fs
\end{code}

\begin{code}
procYicesType :: ProcedureT -> TypY
procYicesType (Procedure {prcdArgs = args, prcdResult = resultType}) = 
  let ytypes = map (basicTypeY . declType) args
  in case resultType of
       NoType -> ARR (ytypes ++ [actionType])
       t -> ARR (ytypes ++ [indexedResult t])
\end{code}

\begin{code}
procConvY :: ProcedureT -> CmdY
procConvY proc =
  DEFINE (prcdName proc, procYicesType proc) (Just $ actionExpr proc)
\end{code}

\begin{code}
maxObjs :: Int
maxObjs = 4
\end{code}

\begin{code}
tagName p = prcdName p ++ "_tag"
tagTypeStr = "proc_tag"
\end{code}

\begin{code}
procTags procs = DEFTYP tagTypeStr typedef
  where typedef = case procs of
          [] -> Nothing
          _ -> Just $ SCALAR $ map tagName procs
\end{code}

\begin{code}
tagArray = DEFINE ("tag_array", ARR [indexType, VarT "proc_tag"]) 
                  Nothing
\end{code}

Group action definition

\begin{code}
actionsDecls = zip ["idx"] listActionsTypes
actionsType = ARR listActionsTypes
listActionsTypes = [indexType, boolTypeY]
\end{code}

\begin{code}
actionOptions procs = 
  let actionsLambda = LAMBDA actionsDecls actionExprs
      actionExprs = if null procs
                    then LitB True 
                    else OR (map runProc procs)
      runProc p = APP (APP (VarE $ prcdName p) (argsFromArray (prcdArgs p))) 
                      [preIdx]
  in DEFINE ("actions", actionsType) (Just actionsLambda)
\end{code}

\begin{code}
argArrayDefines types = 
  let argDef s = DEFINE (argArrayFromName s, argArrayType s) Nothing
      intArgs = DEFINE ("Int_arg", ARR [intTypeY, intTypeY, intTypeY]) Nothing 
  in [intArgs] ++  map (argDef . structName) types
\end{code}

\begin{code}
argArrayFromName n = n ++ "_arg"
argArrayFromType = argArrayFromName . show
argArrayVal = VarE . argArrayFromType
argArrayType s = ARR [intTypeY, indexType, VarT (structStr s)]
\end{code}

\begin{code}
argsFromArray = 
  let go i (Decl _ t) = APP (argArrayVal t) [LitI i, preIdx]
  in zipWith go [1..] 
\end{code}

\end{document}
