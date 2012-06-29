\documentclass {article}
%include polycode.fmt

\begin{document}

\begin{code}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Language.DemonL.YicesDomain (procDom) where

import Control.Monad

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
      mkType (Struct n _) = 
          let t = DATATYPE [(n ++ "_cons", [(n ++ "_id", intTypeY)])]
          in DEFTYP (n ++ "_ref") (Just intTypeY)
      procTagsAndArray  =  [procTags procs, tagArray]
      refDefines        =  map mkType types ++ allType types : argArrayDefines types
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
        mapExpr next = map (exprY thisTime next)
        prePosts = mapExpr thisTime pres ++ map buildPost posts
    in actionBodyLambda $ and' $ prePosts ++ additional
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
  let (e', ExCtx _ record wrap) = runExState $ buildQueryEnv e
  in wrap (AND [e' record, updateTimeCtx nextTime record])

data ExCtx = 
  ExCtx
  { ctxFresh :: Int
  , ctxRecord :: ExpY
  , ctxWrapper :: ExpY -> ExpY
  }

runExState m = runState m $ ExCtx 0 thisTimeRec id

data ExState a = ExState {
  runState :: ExCtx -> (a, ExCtx)
  }

instance Monad ExState where
  return x = ExState $ \s -> (x, s)
  (ExState f) >>= g = ExState $ \s1 ->
    let (x1, s2@(ExCtx i1 r1 wrap1)) = f s1
        (x2, ExCtx i2 r2 wrap2) = runState (g x1) s2
    in (x2, ExCtx i2 r2 wrap2)

instance Functor ExState where
  fmap f mx = mx >>= \x -> return (f x)

setFresh i = ExState $ \s -> ((), s {ctxFresh = i})
getFresh = ExState $ \s -> (ctxFresh s, s)
bumpFresh = fmap (+1) getFresh >>= setFresh

getRecord = ExState $ \s -> (ctxRecord s, s)
setRecord r = ExState $ \s -> ((), s {ctxRecord = r})
addWrap wrap = ExState $ \s -> ((), s {ctxWrapper = ctxWrapper s . wrap})

buildQueryEnv :: TExpr -> ExState (ExpY -> ExpY)
buildQueryEnv = go
  where
    noMod = return . const
    
    go (LitNull t)    = noMod $ nullValue t
    go (LitDouble d)  = noMod $ LitR (toRational d)
    go (LitBool b)    = noMod $ LitB b
    go (LitInt int)   = noMod $ LitI (fromIntegral int)
    go (Var v t)      = noMod $ VarE v
    go (Access e f t) = go (Call f [e] t)
    go (BinOpExpr bop e1 e2 t) = do
      e1' <- go e1
      e2' <- go e2
      return (\x -> binYices bop (e1' x) (e2' x))
    go (UnOpExpr Old e t) = noMod $ exprY thisTime thisTime e
    go (UnOpExpr uop e t) = do
      e' <- go e
      return (unary uop . e')
    go (Call name args t) = 
      let 
        exResult x = "ex_" ++ name ++ "_" ++ show x
        
        updated :: [ExpY -> ExpY] -> Int -> ExpY -> ExpY
        updated args'  fresh record = UPDATE_R record name updateFunc      
             where 
               updateFunc = UPDATE_F (SELECT_R record name)
                                     (argsAt args' record) 
                                     (VarE $ exResult fresh)
        
        argsAt :: [ExpY -> ExpY] -> ExpY -> [ExpY]
        argsAt as record = map ($ record) as

        mkEnv :: ExpY -> Int -> ExpY -> ExpY
        mkEnv record i e = EXISTS [(exResult i, basicTypeY t)] e
        
        final as x = APP (SELECT_R x name) (argsAt as x)
      in do
        bumpFresh
        args' <- mapM go args
        fresh <- getFresh
        record' <- fmap (updated args' fresh) getRecord
        setRecord record'
        addWrap (mkEnv record' fresh)
        return (final args')
        
      -- let freshI = i' + 1
          
      --     envVar = VarE . envName
      --     envName x = "ctx_" ++ show x
          
      --     exResult x = "ex_" ++ name ++ "_" ++ show x
          
      --     argsAt as x = map ($ x) as
          
      --     getFunc x = SELECT_R (envVar x)
      --     updateFunc = UPDATE_F (getFunc i' name)
      --                           (argsAt args' i') 
      --                           (VarE $ exResult freshI)
      --     updated = UPDATE_R (envVar i') name updateFunc
      --     mkEnv e = 
      --       EXISTS [(exResult freshI, basicTypeY t)]
      --              (LET [((envName freshI, Nothing), updated)] e
      -- in ((freshI, env' . mkEnv), \x -> APP (getFunc x name) (argsAt args' x))

unary Not = NOT 
unary Neg = (LitI 0 :-:)

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
