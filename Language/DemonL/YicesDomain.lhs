\documentclass {article}
%include polycode.fmt

\begin{document}

\begin{code}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Language.DemonL.YicesDomain (procDom) where

import Control.Applicative

import Math.SMT.Yices.Syntax

import Text.Parsec.ByteString

import Language.DemonL.AST hiding (Expr (..))
import Language.DemonL.Frame
import Language.DemonL.Parser
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
tagsAndTypes (Domain types procs funcs) = procTagsAndArray ++ refDefines
    where
      procTagsAndArray  =  [procTags procs, tagArray]
      refDefines        =  map structConvY types ++ 
                           [allType types] ++ 
                           argArrayDefines types
\end{code}
Next, the two framing types can be defined.
The first is an exclusion predicate type which
will tell if an element of the ``top'' type is to be
excluded from a full frame-check.
In other words it says if a reference is mutable.

The second frame type is a predicate which will take an
exclusion predicate and a state and ... 
\begin{code}
frameCmds = [excludeTypeDecl, frameTypeDecl]
\end{code}
Now the datatypes can be described in terms of their
component parts.
The types are essentially labelled records,
so we translate their values over time to 
functions of the data structure indexed by time.
\begin{code}
attrFunctions types = concatMap attrConvY types
\end{code}

Additionally, now that the types can be queried and described,
they can be tested for structural equality.

\begin{code}
equalityFunctions types = map structEquals types
\end{code}
The generation of the frames are then fairly straight-forward.
The frame predicates for a type are divided into two pieces,
one half handles the frame for a single reference,
the other maintains that the frame for all references of a type
are maintained at any given time.
\begin{code}
frames types = concatMap bothFrames types ++ [allFrameTypes types]
  where bothFrames t = [frameSingle t ,frameAllObjs t]
\end{code}
The actions that are generated from procedures have to main components:
\begin{itemize}
\item The actions themselves, incorporating the state-change semantics and
  framing rules.
\item The selection of the actions that can be taken at any particular
  time index.
\end{itemize}
\begin{code}
actions types procs = map (procConvY types) procs ++ [actionOptions procs]
\end{code}

Finally, the generation of the Yices statements above can be
brought to gether in the he |procDom| function. 
This is the top level conversion from
a domain to a series of Yices commands.

\begin{code}
procDom :: DomainU -> [CmdY]
procDom untypedDom  = 
    let eiDom = runTypeM $ typecheckDomain untypedDom
    in either (error . ("Error typechecking domain: " ++))
              ( \d@(Domain types procs funcs) ->
                    concat  [ tagsAndTypes d 
                            , frameCmds
                            , attrFunctions types
                            , equalityFunctions types
                            , frames types
                            , functions funcs
                            , actions types procs
                            ]) 
              eiDom
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
        mapExpr next = map (exprY preIdx next)
        prePosts = mapExpr preIdx pres ++ mapExpr postIdx posts
    in actionBodyLambda (and' $ prePosts ++ additional)
\end{code}

\begin{code}
actionBodyLambda = LAMBDA [(idxStr, indexType)]
\end{code}

\begin{code}
actionExpr :: [Struct] -> ProcedureT -> ExpY
actionExpr types proc =
  let
    pres   = clauseExprs (prcdReq proc)
    posts  = clauseExprs (prcdEns proc)
    frame  = actionFrame types proc
    tag    = APP (VarE "tag_array") [preIdx] := VarE (tagName proc)
    body   = actionBody [frame, tag] pres posts
  in LAMBDA (declsToArgsY (prcdArgs proc)) body
\end{code}

\begin{code}
functions fs = 
  let functionDecl f = DEFINE (prcdName f, procYicesType f) Nothing
      function f = ASSERT (VarE (prcdName f) := funcExpr f)
  in map functionDecl fs ++ map function fs
\end{code}

\begin{code}
funcExpr f = 
  let 
    body   = LAMBDA [(idxStr, indexType)] posts
    posts  = 
      case map clauseExpr (prcdEns f) of
        [BinOpExpr (RelOp Eq _) (ResultVar _) e _] -> exprY preIdx preIdx e
        ens -> error $ "Cannot process function ensures: " ++ show ens
  in LAMBDA (declsToArgsY (prcdArgs f)) body
\end{code}

\begin{code}
attrType :: String -> Type -> TypY
attrType structType resultType = 
    ARR  [ basicTypeY (StructType structType [])
         , indexType, 
           basicTypeY resultType]
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
procConvY :: [Struct] -> ProcedureT -> CmdY
procConvY types proc =
  DEFINE (prcdName proc, procYicesType proc) (Just $ actionExpr types proc)
\end{code}

\begin{code}
maxObjs :: Int
maxObjs = 4
\end{code}

\begin{code}
idxRefObj nm i = nm ++ "_obj" ++ show i
allValsOf str = nullStr str :  map (idxRefObj str) [1..maxObjs]
\end{code}

\begin{code}
structConvY (Struct name _) = DEFTYP (structStr name) (Just $ SCALAR objs)
    where objs = nullStr name : map (idxRefObj name) [1 .. maxObjs]
\end{code}

\begin{code}
attrConvY (Struct name decls) = map (declToFunction name) decls
\end{code}

\begin{code}
declToFunction :: String -> Decl -> CmdY
declToFunction typeName (Decl name resultType) =
  DEFINE (name, attrType typeName resultType) Nothing
\end{code}

\begin{code}
tagName p = prcdName p ++ "_tag"
tagType = VarT tagTypeStr
tagTypeStr = "proc_tag"
\end{code}

\begin{code}
procTags procs = DEFTYP tagTypeStr typedef
  where typedef = case procs of
          [] -> Nothing
          ps -> Just $ SCALAR $ map tagName procs
\end{code}

\begin{code}
excludeTypeDecl = DEFTYP "exclude_type" (Just $ ARR [allRefType, indexType, boolTypeY])
tagArray = DEFINE ("tag_array", ARR [indexType, VarT "proc_tag"]) 
                  Nothing
\end{code}
Frame conditions

\begin{code}
excludeType    = VarT "exclude_type"
excludePredE   = VarE excludePredStr
excludePredStr = "exclude_pred"
excludeDecl    = (excludePredStr, excludeType)
\end{code}

\begin{code}
frameType = VarT frameTypeStr
frameTypeStr = "frame_type"
frameTypeDecl = DEFTYP frameTypeStr 
                (Just $ ARR [excludeType, indexType, boolTypeY])
\end{code}

\begin{code}
frameSingle (Struct name _) =
  let
    frameName   = name ++ "_frame_single"
    objType     = VarT $ structStr name
    singleType  = ARR [objType, excludeType, indexType, boolTypeY]
    eq          = APP (VarE $ name ++ "_eq") [objY, preIdx]
    guardFrame  = (NOT $ APP (VarE excludePredStr) [allWrap name objY, preIdx]) :=> eq
    lambda      = LAMBDA [("obj", objType) , excludeDecl, idxDecl] guardFrame
  in DEFINE (frameName, singleType) (Just lambda)
\end{code}

\begin{code}
frameAllObjs (Struct name _) =
  let
    frameName      = name ++ "_frame_all"
    frameType      = ARR [excludeType, indexType, boolTypeY]
    typeEq         = VarE $ name ++ "_frame_single"
    singleFrame n  = APP typeEq [VarE n, excludePredE, preIdx]
    allFrames      = map singleFrame (allValsOf name)
    frameLambda    = LAMBDA [excludeDecl, idxDecl] (and' allFrames)
  in DEFINE (frameName, frameType) (Just frameLambda)
\end{code}

\begin{code}
allFrameTypes types = 
  let
    frameName   = "all-frames"
    frameType   = ARR [excludeType, indexType, boolTypeY]
    typeFrame s = APP (VarE $ structName s ++ "_frame_all") [excludePredE, preIdx]
    allFrames  = map typeFrame types
    lambda     = LAMBDA [excludeDecl, idxDecl] (and' allFrames)
  in DEFINE (frameName, frameType) (Just lambda)
\end{code}

\begin{code}
obj = Var "obj" NoType
objDecl t = ("obj", t)
\end{code}

\begin{code}
attrEq (Decl dn _) = exprY preIdx postIdx e
  where 
    acc e = Access obj e NoType
    e = BinOpExpr (RelOp Eq NoType) 
                  (acc dn) 
                  (UnOpExpr Old (acc dn) NoType) 
                  NoType
\end{code}

\begin{code}
structEquals (Struct name decls) = 
  let
    typ = ARR [objType, indexType, boolTypeY]
    objType = VarT $ structStr name
    lam = LAMBDA [objDecl objType, idxDecl] lamExpr
    lamExpr = and' $ map attrEq decls
  in DEFINE (name ++ "_eq", typ) (Just lam)
\end{code}

\begin{code}
parseErrorStr fn = (++) ("Error parsing demonic domain: " ++ fn ++ "\n")
\end{code}
Group action definition

\begin{code}
actionsDecls = zip ["frm", "idx"] listActionsTypes
actionsType = ARR listActionsTypes
listActionsTypes = [frameType, indexType, boolTypeY]
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
