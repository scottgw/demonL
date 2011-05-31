\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module YicesDomain (generateDomain) where

import Math.SMT.Yices.Syntax

import Text.Parsec.ByteString

import AST
import Parser
import Types
import Yices

clauseExprs = map clauseExpr

-- Action creation
actionType = ARR [indexType, basicTypeY BoolType]

actionBody :: [Expr] -> [Expr] -> ExpY
actionBody pres posts = 
  actionBodyLambda (AND $ map (exprY preIdx) pres ++ map (exprY postIdx) posts)
  
actionBodyLambda = LAMBDA [(idxStr, indexType)]
        
actionExpr :: Procedure Expr -> ExpY
actionExpr (Procedure {prcdArgs = args, prcdReq = req, prcdEns = ens}) =
  let
    pres  = clauseExprs req
    posts = clauseExprs ens
    body  = actionBody pres posts
  in LAMBDA (declsToArgsY args) body

attrType :: String -> Type -> TypY
attrType structType resultType = 
    ARR  [basicTypeY (StructType structType []),
          ARR [indexType, basicTypeY resultType]]

procYicesType :: Procedure Expr -> TypY
procYicesType (Procedure {prcdArgs = args, prcdResult = resultType}) = 
  let ytypes = map (basicTypeY . declType) args
  in  
   case resultType of
     NoType -> ARR (ytypes ++ [actionType])
     _ -> error "only works on proper procedures, without a result"

procConvY :: Procedure Expr -> CmdY
procConvY proc =
  DEFINE (prcdName proc, procYicesType proc) (Just $ actionExpr proc)

maxObjs :: Int
maxObjs = 4

idxRefObj nm i = nm ++ "_obj" ++ show i

structConvY (Struct name _) = DEFTYP (structStr name) (Just $ SCALAR objs)
    where objs = map (idxRefObj name) [1 .. maxObjs]

attrConvY (Struct name decls) = map (declToFunction name) decls

declToFunction :: String -> Decl -> CmdY
declToFunction typeName (Decl name resultType) =
  DEFINE (name, attrType typeName resultType) Nothing
--  where attrFuncName = typeName ++ "_" ++ name

tagName p = prcdName p ++ "_tag"
tagType = VarT tagTypeStr
tagTypeStr = "proc_tag"

procTags procs = DEFTYP tagTypeStr (Just $ SCALAR tags)
  where tags = map tagName procs
        
allRefType = VarT allRefStr
allRefStr = "ALL_ref"

excludeTypeDecl = DEFTYP "exclude_type" (Just $ ARR [allRefType, boolTypeY])
tagArray = DEFTYP "tag_array" 
           (Just $ ARR [intTypeY, indexType, VarT "proc_tag"])




preamble = [excludeTypeDecl, frameTypeDecl, tagArray]

allType types = DEFTYP allRefStr (Just $ DATATYPE $ map mkTyCon types)
  where mkTyCon typ = let n = structName typ
                      in (n, [(structStr n, VarT (structStr n))])
          

procDom :: Domain -> [CmdY]
procDom (Domain procs types) = 
    let actions  = map procConvY procs 
        argArrays = argArrayDefines types
        actionSel = actionOptions procs
        attrs    = concatMap attrConvY types
        refTypes = map structConvY types
        eqs      = map structEquals types
        frames   = map frameAllObjs types
    in concat [[procTags procs]
              ,refTypes
              ,argArrays
              ,[allType types]
              ,preamble
              ,attrs
              ,eqs
              ,frames
              ,actions
              ,[actionSel]
              ]

-- Frame conditions
excludeType    = VarT "exclude_type"
excludePredE   = VarE excludePredStr
excludePredStr = "exclude_pred"
excludeDecl    = (excludePredStr, excludeType)

frameType = VarT frameTypeStr
frameTypeStr = "frame_type"
frameTypeDecl = DEFTYP frameTypeStr 
                (Just $ ARR [excludeType, indexType, boolTypeY])



frameAllObjs (Struct name _) =
  let
    frameName     = name ++ "_frame_all"
    typeEq        = VarE $ name ++ "_eq"
    singleFrame i = APP typeEq [excludePredE, VarE (idxRefObj name i), preIdx]
    allFrames     = map singleFrame [1 .. maxObjs]
    frameLambda   = LAMBDA [excludeDecl, idxDecl] (AND allFrames)
  in DEFINE (frameName, excludeType) (Just frameLambda)

obj = Var "obj"
objDecl t = ("obj", t)

attrEq (Decl dn _) = exprY postIdx e
  where 
    acc = Access obj
    e = BinOpExpr (RelOp Eq NoType) (acc dn) (UnOpExpr Old $ acc dn)

structEquals (Struct name decls) = 
  let
    typ = ARR [objType, indexType, boolTypeY]
    objType = VarT $ structStr name
    
    lam = LAMBDA [objDecl objType, idxDecl] lamExpr

    lamExpr = AND $ map attrEq decls
  in DEFINE (name ++ "_eq", typ) (Just lam)
        

parseErrorStr fn = (++) ("Error parsing demonic domain: " ++ fn ++ "\n")

-- Group action definition

actionsDecls = zip ["tag", "frm", "idx"] listActionsTypes
actionsType = ARR listActionsTypes
listActionsTypes = [tagType, frameType, indexType, boolTypeY]

actionOptions procs = 
  let actionsLambda = LAMBDA actionsDecls actionExprs
      actionExprs = OR $ map actionExpr procs
      actionExpr p = AND [tagMatch p, runProc p]
      tagMatch p = VarE "tag" := VarE (tagName p)
      runProc p = APP (VarE $ prcdName p) (argsFromArray (prcdArgs p))
  in DEFINE ("actions", actionsType) (Just actionsLambda)

argArrayDefines types = 
  let argDef s = DEFINE (argArrayFromName s, argArrayType s) Nothing
  in map (argDef . structName) types

argArrayFromName n = n ++ "_arg"
argArrayFromType = argArrayFromName . show
argArrayVal = VarE . argArrayFromType
argArrayType s = ARR [intTypeY, indexType, VarT (structStr s)]

argsFromArray = 
  let go i (Decl _ t) = APP (argArrayVal t) [LitI i, preIdx]
  in zipWith go [1..] 

generateDomain fileName = do
  domE <- parseFromFile domain fileName
  either 
    (error . parseErrorStr fileName  . show)
    (writeYices fileName . procDom)
    domE
    
\end{code}