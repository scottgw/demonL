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

declsToArgsY :: [Decl] -> [(String, TypY)]
declsToArgsY = concatMap declY
  where declY (Decl name typ) = [(name, basicTypeY typ)]

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

structConvY (Struct name _) = DEFTYP (structName name) (Just $ SCALAR objs)
    where objs = map (idxRefObj name) [1 .. maxObjs]

attrConvY (Struct name decls) = map (declToFunction name) decls

declToFunction :: String -> Decl -> CmdY
declToFunction typeName (Decl name resultType) =
  DEFINE (attrFuncName, attrType typeName resultType) Nothing
  where attrFuncName = typeName ++ "_" ++ name

tagName p = prcdName p ++ "_tag"

procTags procs = DEFTYP "proc_tag" (Just $ SCALAR tags)
  where tags = map tagName procs
        
allRefType = VarT "ALL_ref"

excludeTypeDecl = DEFTYP "exclude_type" (Just $ ARR [allRefType, boolTypeY])
tagArray = DEFTYP "tag_array" 
           (Just $ ARR [intTypeY, indexType, VarT "proc_tag"])

preamble = [excludeTypeDecl, tagArray]

procDom :: Domain -> [CmdY]
procDom (Domain procs types) = 
    let actions  = map procConvY procs 
        attrs    = concatMap attrConvY types
        refTypes = map structConvY types
        eqs      = map structEquals types
        frames   = map frameAllObjs types
    in preamble ++ [procTags procs] ++ 
       concat [refTypes
              ,attrs
              ,eqs
              ,frames
              ,actions
              ]

-- Frame conditions
excludeType    = VarT "exclude_type"
excludePredE   = VarE excludePredStr
excludePredStr = "exclude_pred"
excludeDecl    = (excludePredStr, excludeType)

frameAllObjs (Struct name decls) =
  let
    frameName     = name ++ "_frame_all"
    typeEq        = VarE $ name ++ "_eq"
    singleFrame i = APP typeEq [excludePredE, VarE (idxRefObj name i), preIdx]
    allFrames     = map singleFrame [1 .. maxObjs]
    frameLambda   = LAMBDA [excludeDecl, idxDecl] (AND allFrames)
  in DEFINE (frameName, excludeType) (Just frameLambda)

structEquals (Struct name decls) = 
  let
    obj = Var "obj"
    accessObj dn = Access obj dn
    eqExpr (Decl dn _) = BinOpExpr (RelOp Eq NoType)
                                   (accessObj dn)
                                   (UnOpExpr Old $ accessObj dn)
    eqAttrs = map eqExpr decls
    lamExpr = actionBodyLambda $ AND $ map (exprY postIdx) eqAttrs
  in DEFINE (name ++ "_eq", actionType) (Just lamExpr)
        
outputFileName fn = fn ++ ".lisp"
showDomain = unlines . map show . procDom

generateDomain fileName = do
  domE <- parseFromFile domain fileName
  either 
    (\e -> putStrLn ("Error parsing demonic domain: " ++ fileName) >> print e)
    (\d -> writeFile (outputFileName fileName) (showDomain d))
    domE