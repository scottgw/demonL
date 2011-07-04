module TypeCheck where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Error

import Data.Functor.Identity
import Data.List
import qualified Data.Map as M

import qualified AST as A
import AST (Struct (..), ProcedureU, Procedure (..), Clause (..), declsToMap,
            Domain (..), DomainU, BinOp (..), UnOp (..), ROp (..))
import Types

type TypeM a = ErrorT String Identity a

type ProcedureT = Procedure TExpr
type DomainT = Domain TExpr

data TExpr = 
    Call String [TExpr] Type
  | BinOpExpr BinOp TExpr TExpr Type
  | UnOpExpr UnOp TExpr Type
  | Access TExpr String Type
  | Var String Type
  | ResultVar Type
  | Cast TExpr Type
  | LitInt Int
  | LitBool Bool
  | LitNull Type
  | LitDouble Double deriving (Show, Eq)

runTypeM = runIdentity . runErrorT

texprType (Call _ _ t) = t
texprType (BinOpExpr _ _ _ t) = t
texprType (UnOpExpr _ _ t) = t
texprType (Access _ _ t) = t
texprType (Var _ t) = t
texprType (ResultVar t) = t
texprType (Cast _ t) = t
texprType (LitInt _) = IntType
texprType (LitBool _) = BoolType
texprType (LitNull t) = t
texprType (LitDouble _) = DoubleType 



typecheckDomain :: DomainU -> TypeM DomainT
typecheckDomain (Domain types procs) = 
    Domain <$> pure types <*> mapM (typecheckProc types) procs

typecheckProc :: [Struct] -> ProcedureU -> TypeM ProcedureT
typecheckProc types (Procedure name args res req ens) =
    let argMap = declsToMap args
        tcClause = typecheckClause argMap types
        req' = mapM tcClause req
        ens' = mapM tcClause ens
    in Procedure name args res <$> req' <*> ens'


typecheckClause argMap types (Clause n e) = 
    Clause n <$> typecheckExpr argMap types e

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

unsafeCheck decls types e = 
    let te = runTypeM $ typecheckExpr (declsToMap decls) types e
    in either error id te

typecheckExpr :: M.Map String Type -> [Struct] -> A.Expr -> TypeM TExpr
typecheckExpr argMap types = 
    let tc :: A.Expr -> TypeM TExpr
        tc (A.Access e attr) = 
            let eM         = tc e
                structStr  = getStructNameM =<< (texprType <$> eM)
                attrTypM   = lookupAttrType types attr =<< structStr
            in Access <$> eM <*> pure attr <*> attrTypM
        tc (A.Var n) = Var n <$> lookupName n argMap
        tc (A.LitInt i) = pure $ LitInt i
        tc (A.LitBool b) = pure $ LitBool b
        
        -- both equality and inequality are dealt with specially
        -- as they are needed to type `null' values.
        tc (A.BinOpExpr bOp@(RelOp Eq _) A.LitNull A.LitNull) = 
          pure $ BinOpExpr bOp (LitNull VoidType) (LitNull VoidType) BoolType
        tc (A.BinOpExpr bOp@(RelOp Eq _) A.LitNull e) =
          let eM     = tc e
              nullM  = LitNull <$> (texprType <$> eM)
          in  BinOpExpr bOp <$> nullM <*> eM <*> pure BoolType
        tc (A.BinOpExpr bOp@(RelOp Eq _) e A.LitNull) =
          let eM     = tc e
              nullM  = LitNull <$> (texprType <$> eM)
          in  BinOpExpr bOp <$> eM <*> nullM <*> pure BoolType
              
        tc (A.BinOpExpr bOp@(RelOp Neq _) A.LitNull A.LitNull) = 
          pure $ BinOpExpr bOp (LitNull VoidType) (LitNull VoidType) BoolType
        tc (A.BinOpExpr bOp@(RelOp Neq _) A.LitNull e) =
          let eM     = tc e
              nullM  = LitNull <$> (texprType <$> eM)
          in  BinOpExpr bOp <$> nullM <*> eM <*> pure BoolType
        tc (A.BinOpExpr bOp@(RelOp Neq _) e A.LitNull) =
          let eM     = tc e
              nullM  = LitNull <$> (texprType <$> eM)
          in  BinOpExpr bOp <$> eM <*> nullM <*> pure BoolType


        tc (A.BinOpExpr bOp e1 e2) =
            let e1M  = tc e1
                e2M  = tc e2
                tM   = join $ binOpTypes bOp <$> e1M <*> e2M
            in BinOpExpr bOp <$> e1M <*> e2M <*> tM
        tc (A.UnOpExpr uop e) = 
            let eM  = tc e
                tM  = join $ unOpTypes uop <$> eM
            in UnOpExpr uop <$> eM <*> tM
        tc e = throwError $ "Can't typecheck " ++ show e
    in tc

isBool And      = True
isBool Or       = True
isBool Implies  = True
isBool _        = False

isNumType IntType = True
isNumType DoubleType = True
isNumType _ = False

isNum Add = True
isNum Sub = True
isNum Mul = True
isNum Div = True
isNum _   = False

unOpTypes Not e 
    | texprType e == BoolType  = return BoolType
unOpTypes Neg e
    | isNumType (texprType e)  = return $ texprType e
unOpTypes Old e = return $ texprType e

binOpTypes :: BinOp -> TExpr -> TExpr -> TypeM Type
binOpTypes (RelOp r _) e1 e2 
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