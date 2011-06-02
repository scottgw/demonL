\documentclass {article}
%include polycode.fmt

\begin{document}

\begin{code}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module YicesDomain (generateDomain) where

import Control.Applicative

import Control.Monad.Trans.Error

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

import Math.SMT.Yices.Syntax

import Text.Parsec.ByteString

import AST hiding (Expr (..))
import Parser
import Types
import TypeCheck
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
    let Right d@(Domain procs types) = runTypeM $ typecheckDomain untypedDom
    in concat  [ tagsAndTypes d 
               , frameCmds
               , attrFunctions types
               , equalityFunctions types
               , frames types
               , actions types procs
               ]
\end{code}


Action Creation

The action type is the result type of any action,
namely a predicate on the state index to a boolean.
This represents that an action can be true in a particular state.
\begin{code}
actionType = ARR [indexType, basicTypeY BoolType]
\end{code}
\begin{code}
actionBody :: ExpY -> [TExpr] -> [TExpr] -> ExpY
actionBody frame pres posts =
    let prePosts      = map (exprY preIdx) pres ++ map (exprY postIdx) posts
    in actionBodyLambda (AND $ prePosts ++ [frame])
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
        args = prcdArgs proc
        getName var = 
            case find (\ (Decl n _) -> n == var) args of
              Just (Decl _ (StructType sName _))  -> Just sName
              _                                   -> Nothing
        getStruct var = 
            case getName var of 
              Just sName  -> find ((== sName) . structName) types
              _           -> Nothing

        insertStruct (Struct n attrs) = M.insert n (declsToMap attrs)
        typesToMap = foldr insertStruct M.empty types

        removeModified sName attr = M.adjust (M.delete attr) sName
        updateModified modMap e attr = 
            case texprType e of
              StructType sName _ -> removeModified sName attr modMap
              _  -> modMap
        go (LitInt _) modMap = modMap
        go (Access e attr _) modMap = updateModified modMap e attr
        unmodifiedMap = foldr go typesToMap (clauseExprs $ prcdEns proc)
    in error $ show unmodifiedMap
\end{code}

\begin{code}
actionBodyLambda = LAMBDA [(idxStr, indexType)]
\end{code}

\begin{code}
clauseExprs = map clauseExpr
\end{code}

\begin{code}
actionExpr :: [Struct] -> ProcedureT -> ExpY
actionExpr types proc =
  let
    pres   = clauseExprs (prcdReq proc)
    posts  = clauseExprs (prcdEns proc)
    frame  = actionFrame types proc
    body   = actionBody frame pres posts
  in LAMBDA (declsToArgsY (prcdArgs proc)) body
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
  in  
   case resultType of
     NoType -> ARR (ytypes ++ [actionType])
     _ -> error "only works on proper procedures, without a result"
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
                      in (allWrapStr n, [(allUnwrapStr n, VarT (structStr n))])
allUnwrapStr str = structStr str ++ "_unwrap"
allWrapStr str = structStr str ++ "_wrap"
allWrap str v = APP (VarE $ allWrapStr str) [v]
\end{code}

\begin{code}
excludeTypeDecl = DEFTYP "exclude_type" (Just $ ARR [allRefType, boolTypeY])
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
    obj         = VarE "obj"
    singleType  = ARR [objType, excludeType, indexType, boolTypeY]
    eq          = APP (VarE $ name ++ "_eq") [obj, preIdx]
    guardFrame  = APP (VarE excludePredStr) [allWrap name obj] :=> eq
    lambda      = LAMBDA [("obj", objType) , excludeDecl, idxDecl] guardFrame
  in DEFINE (frameName, singleType) (Just lambda)
\end{code}

\begin{code}
frameAllObjs (Struct name _) =
  let
    frameName      = name ++ "_frame_all"
    frameType      = ARR [excludeType, indexType, boolTypeY]
    typeEq         = VarE $ name ++ "_frame_single"
    singleFrame i  = APP typeEq [VarE (idxRefObj name i), excludePredE, preIdx]
    allFrames      = map singleFrame [1 .. maxObjs]
    frameLambda    = LAMBDA [excludeDecl, idxDecl] (AND allFrames)
  in DEFINE (frameName, frameType) (Just frameLambda)
\end{code}

\begin{code}
allFrameTypes types = 
  let
    frameName   = "all-frames"
    frameType   = ARR [excludeType, indexType, boolTypeY]
    typeFrame s = APP (VarE $ structName s ++ "_frame_all") [excludePredE, preIdx]
    allFrames  = map typeFrame types
    lambda     = LAMBDA [excludeDecl, idxDecl] (AND allFrames)
  in DEFINE (frameName, frameType) (Just lambda)
\end{code}

\begin{code}
obj = Var "obj" NoType
objDecl t = ("obj", t)
\end{code}

\begin{code}
attrEq (Decl dn _) = exprY postIdx e
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
      runProc p = APP (APP (VarE $ prcdName p) (argsFromArray (prcdArgs p))) 
                      [preIdx]
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
  let writeAndProcess dom = (dom,) <$>  writeYices fileName (procDom dom)
  either (error . parseErrorStr fileName  . show)
         writeAndProcess
         domE
\end{code}
\end{document}