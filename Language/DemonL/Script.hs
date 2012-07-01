{-# LANGUAGE TupleSections #-}

module Language.DemonL.Script where

import Control.Applicative
import Control.Monad

import Data.List
import qualified Data.Map as M

import Math.SMT.Yices.Syntax

import Language.DemonL.AST
import Language.DemonL.Goal
import Language.DemonL.TypeCheck (ProcedureT)

generateScript goal dom goalExprs = 
    let actionMap = foldr inspectTag M.empty goalExprs
        argumentMap = foldr (inspectArg actionMap dom) M.empty goalExprs
        equivs = objEquivalence goal goalExprs
    in reconstrFromMaps dom actionMap argumentMap equivs

reconstrFromMaps dom actMap argMap equivs = 
    let f idx proc = proc ++ " (" ++ 
                     reconstrArgs (numArgs dom proc) idx equivs argMap ++ 
                     ")"

        scriptMap = M.mapWithKey f actMap
    in unlines $ map snd $ M.toAscList scriptMap

objEquivalence :: SerialGoal -> [ExpY] -> M.Map Integer String
objEquivalence goal goalExprs =
    let f ((VarE v) := (LitI i)) m 
            | isGoalVar v goal = M.insert i v m
        f ((LitI i) := (VarE v)) m 
            | isGoalVar v goal = M.insert i v m
        f _ m = m
    in foldr f M.empty goalExprs

numArgs dom name = 
  case findProc dom name  of
    Just p -> length (prcdArgs p)
    Nothing -> error $ "no proc " ++ name ++ " found"

isGoalVar v goal = v `elem` map declName (vars goal)

-- | Reconstruct the comma separated argument string.
reconstrArgs :: Int -> Integer -> M.Map Integer String 
             -> M.Map Integer (M.Map Integer ExpY) -> String
reconstrArgs n idx equivs = 
    let exprEquiv (LitI i) = maybe (show i) id (M.lookup i equivs)
        exprEquiv e = show e
    in intercalate "," . take n . map (exprEquiv . snd) . M.toAscList . (M.! idx)

inspectArg actionMap dom (e1 := e2) argsMap = 
    let 
        argTuple = argM actionMap dom e1 <|> argM actionMap dom e2
        val = valM actionMap dom e1 <|> valM actionMap dom e2
        insertArg (name, timeIndex, argNum) v =
            M.insertWith M.union timeIndex (M.singleton argNum v) argsMap
        argsMapM = insertArg <$> argTuple <*> val
    in maybe argsMap id argsMapM

valM actionMap dom e = 
  case argM actionMap dom e of
    Just _ -> Nothing
    _ -> Just e

argStr = "_arg"


-- | Given an action map, a domain and a Yices expression,
-- build a type argument number and index
argM actionMap dom (APP (VarE str) [LitI timeIndex, LitI argNum]) = 
  let proc = findProcUnsafe dom (actionMap M.! timeIndex)
      args = prcdArgs proc
      argDecl = args !! fromIntegral argNum
      argTypeName = show $ declType argDecl
  in if fromIntegral argNum < length args && argTypeName `isPrefixOf` str 
     then Just (argTypeName, timeIndex, argNum)
     else Nothing
argM _ _ _ = Nothing

inspectTag (e1 := e2) tagMap = 
    let tagNum   = tagNumM e1 <|> tagNumM e2
        tag      = tagM e1 <|> tagM e2
        tagMapM  = M.insert <$> tagNum <*> tag <*> pure tagMap
    in maybe tagMap id tagMapM

tagStr = "_tag"

stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

tagM (VarE str) = stripSuffix tagStr str
tagM _ = Nothing

tagNumM (APP (VarE "tag_array") [LitI i]) = Just i
tagNumM _ = Nothing
