\documentclass {article}
%include polycode.fmt

\begin{document}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module YicesDomain (generateDomain) where

import Math.SMT.Yices.Syntax

import Text.Parsec.ByteString

import AST
import Parser
import Types
import Yices
\end{code}

The procedure tags (allowing us to track which actions were used)
and reference type definitions are not dependent on
other declarations in the file, 
so they will be given first.

The reference type declarations also require the generation of the
``top'' type which can be any of the reference types.
For simplicity, we also generate the argument array definitions as well.
\begin{code}
tagsAndTypes (Domain procs types) = procTagsAndArray ++ refDefines
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
\begin{code}
frames types = map frameAllObjs types
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
procDom :: Domain -> [CmdY]
procDom d@(Domain procs types) = 
  concat  [ tagsAndTypes d 
          , frameCmds
          , attrFunctions types
          , equalityFunctions types
          , frames types
          , actions procs
          ]
\end{code}


\begin{code}
clauseExprs = map clauseExpr
\end{code}


Action Creation

The action type is the result type of any action,
namely a predicate on the state index to a boolean.
This represents that an action can be true in a particular state.
\begin{code}
actionType = ARR [indexType, basicTypeY BoolType]
\end{code}
\begin{code}
actionBody :: [Expr] -> [Expr] -> ExpY
actionBody pres posts = 
  actionBodyLambda (AND $ map (exprY preIdx) pres ++ map (exprY postIdx) posts)
\end{code}

\begin{code}
actionBodyLambda = LAMBDA [(idxStr, indexType)]
\end{code}

\begin{code}
actionExpr :: Procedure Expr -> ExpY
actionExpr (Procedure {prcdArgs = args, prcdReq = req, prcdEns = ens}) =
  let
    pres  = clauseExprs req
    posts = clauseExprs ens
    body  = actionBody pres posts
  in LAMBDA (declsToArgsY args) body
\end{code}

\begin{code}
attrType :: String -> Type -> TypY
attrType structType resultType = 
    ARR  [basicTypeY (StructType structType []),
          ARR [indexType, basicTypeY resultType]]
\end{code}

\begin{code}
procYicesType :: Procedure Expr -> TypY
procYicesType (Procedure {prcdArgs = args, prcdResult = resultType}) = 
  let ytypes = map (basicTypeY . declType) args
  in  
   case resultType of
     NoType -> ARR (ytypes ++ [actionType])
     _ -> error "only works on proper procedures, without a result"
\end{code}

\begin{code}
procConvY :: Procedure Expr -> CmdY
procConvY proc =
  DEFINE (prcdName proc, procYicesType proc) (Just $ actionExpr proc)
\end{code}

\begin{code}
maxObjs :: Int
maxObjs = 4
\end{code}

\begin{code}
idxRefObj nm i = nm ++ "_obj" ++ show i
\end{code}

\begin{code}
structConvY (Struct name _) = DEFTYP (structStr name) (Just $ SCALAR objs)
    where objs = map (idxRefObj name) [1 .. maxObjs]
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
procTags procs = DEFTYP tagTypeStr (Just $ SCALAR tags)
  where tags = map tagName procs
\end{code}

\begin{code}
allRefType = VarT allRefStr
allRefStr = "ALL_ref"
allType types = DEFTYP allRefStr (Just $ DATATYPE $ map mkTyCon types)
  where mkTyCon typ = let n = structName typ
                      in (n, [(structStr n, VarT (structStr n))])
\end{code}

\begin{code}
excludeTypeDecl = DEFTYP "exclude_type" (Just $ ARR [allRefType, boolTypeY])
tagArray = DEFTYP "tag_array" 
           (Just $ ARR [intTypeY, indexType, VarT "proc_tag"])
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
frameAllObjs (Struct name _) =
  let
    frameName     = name ++ "_frame_all"
    typeEq        = VarE $ name ++ "_eq"
    singleFrame i = APP typeEq [excludePredE, VarE (idxRefObj name i), preIdx]
    allFrames     = map singleFrame [1 .. maxObjs]
    frameLambda   = LAMBDA [excludeDecl, idxDecl] (AND allFrames)
  in DEFINE (frameName, excludeType) (Just frameLambda)
\end{code}

\begin{code}
obj = Var "obj"
objDecl t = ("obj", t)
\end{code}

\begin{code}
attrEq (Decl dn _) = exprY postIdx e
  where 
    acc = Access obj
    e = BinOpExpr (RelOp Eq NoType) (acc dn) (UnOpExpr Old $ acc dn)
\end{code}

\begin{code}
structEquals (Struct name decls) = 
  let
    typ = ARR [objType, indexType, boolTypeY]
    objType = VarT $ structStr name
    lam = LAMBDA [objDecl objType, idxDecl] lamExpr
    lamExpr = AND $ map attrEq decls
  in DEFINE (name ++ "_eq", typ) (Just lam)
\end{code}

\begin{code}
parseErrorStr fn = (++) ("Error parsing demonic domain: " ++ fn ++ "\n")
\end{code}
Group action definition

\begin{code}
actionsDecls = zip ["tag", "frm", "idx"] listActionsTypes
actionsType = ARR listActionsTypes
listActionsTypes = [tagType, frameType, indexType, boolTypeY]
\end{code}

\begin{code}
actionOptions procs = 
  let actionsLambda = LAMBDA actionsDecls actionExprs
      actionExprs = OR $ map actionExpr procs
      actionExpr p = AND [tagMatch p, runProc p]
      tagMatch p = VarE "tag" := VarE (tagName p)
      runProc p = APP (VarE $ prcdName p) (argsFromArray (prcdArgs p))
  in DEFINE ("actions", actionsType) (Just actionsLambda)
\end{code}

\begin{code}
argArrayDefines types = 
  let argDef s = DEFINE (argArrayFromName s, argArrayType s) Nothing
  in map (argDef . structName) types
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

\begin{code}
generateDomain fileName = do
  domE <- parseFromFile domain fileName
  either 
    (error . parseErrorStr fileName  . show)
    (writeYices fileName . procDom)
    domE
\end{code}
\end{document}