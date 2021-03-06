module Language.DemonL.TypeCheck where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Error

import Data.Functor.Identity
import Data.List
import qualified Data.Map as M

import qualified Language.DemonL.AST as A
import Language.DemonL.AST 
            (Struct (..), ProcedureU, Procedure (..), Clause (..), declsToMap,
            Domain (..), DomainU, UnOp (..), ROp (..), Decl (..))
import Language.DemonL.Types

type TypeM a = ErrorT String Identity a

type ProcedureT = Procedure TExpr
type DomainT = Domain TExpr

data TExpr = 
    Call String [TExpr] Type
  | BinOpExpr BinOp TExpr TExpr Type
  | UnOpExpr UnOp TExpr Type
  | ForAll [Decl] TExpr
  | Access TExpr String Type
  | Var String Type
  | ResultVar Type
  | Cast TExpr Type
  | LitInt Integer
  | LitBool Bool
  | LitNull Type
  | LitDouble Double deriving (Show, Eq, Ord)


data BinOp = Add
           | Sub
           | Mul
           | Div
           | Or
           | And
           | Implies
           | ArrayIndex
           | RelOp ROp Type
             deriving (Eq, Ord)

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Or  = "or"
  show And = "and"
  show Implies = "implies"
  show (RelOp op _) = show op

runTypeM = runIdentity . runErrorT

texprType (Call _ _ t) = t
texprType (BinOpExpr _ _ _ t) = t
texprType (UnOpExpr _ _ t) = t
texprType (ForAll _ _) = BoolType
texprType (Access _ _ t) = t
texprType (Var _ t) = t
texprType (ResultVar t) = t
texprType (Cast _ t) = t
texprType (LitInt _) = IntType
texprType (LitBool _) = BoolType
texprType (LitNull t) = t
texprType (LitDouble _) = DoubleType 



typecheckDomain :: DomainU -> TypeM DomainT
typecheckDomain d@(Domain types procs funcs) = 
    Domain <$> pure types 
           <*> mapM (typecheckProc d) procs
           <*> mapM (typecheckProc d) funcs

typecheckProc :: DomainU -> ProcedureU -> TypeM ProcedureT
typecheckProc dom (Procedure name args res req ens) =
    let argMap = declsToMap args
        tcClause = typecheckClause argMap dom res
        req' = mapM tcClause req
        ens' = mapM tcClause ens
    in Procedure name args res <$> req' <*> ens'


typecheckClause argMap dom res (Clause n e) = 
    Clause n <$> typecheckExpr argMap dom res e

noStructErr struct = throwError $ "<" ++ struct ++ "> not found"
noAttrErr struct attr = 
    throwError $ "<" ++ struct ++ ">." ++ attr ++ " not found"


lookupName :: String -> M.Map String Type -> TypeM Type
lookupName str decls = maybe (throwError $ str ++ " not found in declarations")
                             return
                             (M.lookup str decls)

lookupAttrType :: [Struct] -> String -> String -> TypeM Type
lookupAttrType types attr struct =
    let isStruct  = (== struct) . structName
        mbStruct  = find isStruct types
        getAttr   = lookupName attr . declsToMap . structDecls
    in maybe (noStructErr struct) getAttr mbStruct

getStructNameM :: Type -> TypeM String
getStructNameM (StructType n _) = return n
getStructNameM t = throwError $ show t ++ " not a struct"

unsafeCheck decls dom e = 
    let te = runTypeM $ typecheckExpr (declsToMap decls) dom NoType e
    in either error id te

findFunction name dom = 
  let fns = domFuncs dom
      p f = prcdName f == name
  in find p fns

checkFunctionArgs argMap dom resType f args
  | length (prcdArgs f) /= length args = 
    throwError $ "Arguments not same length"
  | otherwise = do
    let formalTypes = map declType (prcdArgs f)
        guardTypeM type1 type2 
           | type1 == type2 = return ()
           | otherwise  = throwError $ "Unconforming arguments: " ++ 
                           show type1 ++ " and " ++ show type2
    actualArgs <- mapM (fmap texprType . typecheckExpr argMap dom resType) args
    zipWithM_ guardTypeM formalTypes actualArgs
    return (prcdResult f)
    
typecheckExpr :: M.Map String Type -> DomainU -> Type -> A.Expr -> TypeM TExpr
typecheckExpr argMap dom resType = 
    let 
      types = domStructs dom
      tc :: A.Expr -> TypeM TExpr
      tc (A.Access e attr) = 
        let eM         = tc e
            structStr  = getStructNameM =<< (texprType <$> eM)
            attrTypM   = lookupAttrType types attr =<< structStr
        in Access <$> eM <*> pure attr <*> attrTypM
      tc (A.Var n) = Var n <$> lookupName n argMap
      tc (A.LitInt i) = pure $ LitInt i
      tc (A.LitBool b) = pure $ LitBool b
      tc A.ResultVar  = pure $ ResultVar resType
      tc (A.Call name args) = 
        case findFunction name dom of
          Just f -> let argsM = mapM (typecheckExpr argMap dom resType) args 
                    in Call name <$> argsM
                                 <*> checkFunctionArgs argMap dom resType f args
          Nothing -> throwError $ "Coudln't find function " ++ name
      
      -- both equality and inequality are dealt with specially
      -- as they are needed to type `null' values.
      tc (A.BinOpExpr (A.RelOp Eq) A.LitNull A.LitNull) = 
        pure $ BinOpExpr (RelOp Eq VoidType) (LitNull VoidType) 
                                             (LitNull VoidType) BoolType
      tc (A.BinOpExpr (A.RelOp Eq) A.LitNull e) =
        let eM     = tc e
            nullM  = LitNull <$> (texprType <$> eM)
        in  BinOpExpr <$> (RelOp Eq <$> (texprType <$> eM))
                      <*> nullM <*> eM <*> pure BoolType
      tc (A.BinOpExpr (A.RelOp Eq) e A.LitNull) =
        let eM     = tc e
            nullM  = LitNull <$> (texprType <$> eM)
        in  BinOpExpr <$> (RelOp Eq <$> (texprType <$> eM)) 
                      <*> eM <*> nullM <*> pure BoolType
            
      tc (A.BinOpExpr (A.RelOp A.Neq) A.LitNull A.LitNull) = 
        pure $ BinOpExpr (RelOp Neq VoidType) (LitNull VoidType) 
                                              (LitNull VoidType) BoolType
      tc (A.BinOpExpr bOp@(A.RelOp Neq) A.LitNull e) =
        let eM     = tc e
            nullM  = LitNull <$> (texprType <$> eM)
        in  BinOpExpr <$> (RelOp Neq <$> (texprType <$> eM))
                      <*> nullM <*> eM <*> pure BoolType
      tc (A.BinOpExpr bOp@(A.RelOp Neq) e A.LitNull) =
        let eM     = tc e
            nullM  = LitNull <$> (texprType <$> eM)
        in  BinOpExpr <$> (RelOp Neq <$> (texprType <$> eM)) 
                      <*> eM <*> nullM <*> pure BoolType
      tc (A.BinOpExpr bOp e1 e2) =
        let e1M  = tc e1
            e2M  = tc e2
            tM   = join $ binOpTypes bOp <$> e1M <*> e2M
        in BinOpExpr (typedBinOp bOp) <$> e1M <*> e2M <*> tM
      tc (A.UnOpExpr uop e) = 
        let eM  = tc e
            tM  = join $ unOpTypes uop <$> eM
        in UnOpExpr uop <$> eM <*> tM
      tc (A.ForAll ds e) = 
        ForAll ds <$> typecheckExpr ds' dom BoolType e
        where
          ds' = declsToMap ds `M.union` argMap
      tc e = throwError $ "Can't typecheck " ++ show e
    in tc

typedBinOp op =
  case op of
    A.Add -> Add
    A.Sub -> Sub
    A.Mul -> Mul
    A.Div -> Div
    A.Or -> Or
    A.And -> And
    A.Implies -> Implies
    A.ArrayIndex -> error "typedBinOp: no array indexes"
    A.RelOp rop -> RelOp (typedROp rop) VoidType

typedROp rop = 
  case rop of
    A.Lte -> Lte
    A.Lt -> Lt
    A.Eq -> Eq
    A.Gt -> Gt
    A.Gte -> Gte
    
isBool A.And      = True
isBool A.Or       = True
isBool A.Implies  = True
isBool _        = False

isNumType IntType = True
isNumType DoubleType = True
isNumType _ = False

isNum A.Add = True
isNum A.Sub = True
isNum A.Mul = True
isNum A.Div = True
isNum _   = False

unOpTypes Not e 
    | texprType e == BoolType  = return BoolType
unOpTypes Neg e
    | isNumType (texprType e)  = return $ texprType e
unOpTypes Old e = return $ texprType e
unOpTypes op  e = error $ "unOpTypes: " ++ show op ++ ", " ++ show e

binOpTypes :: A.BinOp -> TExpr -> TExpr -> TypeM Type
binOpTypes (A.RelOp r) e1 e2 
  | texprType e1 == texprType e2 = pure BoolType
  | otherwise =  throwError $ 
                 show (e1,e2) ++ " are unsuitable arguments for " ++ show r
binOpTypes op e1 e2
    | isNum op = case (texprType e1, texprType e2) of
                     (IntType, IntType) -> return IntType
                     (DoubleType, DoubleType) -> return DoubleType
                     _ -> throwError $ show (e1,e2) ++ 
                                       " are unsuitable arguments for " ++ 
                                       show op
    | isBool op =  case (texprType e1, texprType e2) of
                     (BoolType, BoolType) -> return BoolType
                     _ -> throwError $ show (e1,e2) ++ 
                                       " are unsuitable arguments for " ++ 
                                       show op
