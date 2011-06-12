{-# LANGUAGE TupleSections #-}

module Script where

import Control.Applicative
import Control.Monad

import Data.List
import qualified Data.Map as M

import Math.SMT.Yices.Syntax

import AST
import Goal

generateScript goal dom goalExprs = 
    let actionMap = foldr inspectTag M.empty goalExprs
        argumentMap = foldr inspectArg M.empty goalExprs
        equivs = objEquivalence goal goalExprs
    in reconstrFromMaps dom actionMap argumentMap equivs

reconstrFromMaps dom actMap argMap equivs = 
    let f idx proc = proc ++ " (" ++ reconstrArgs (numArgs dom proc) idx equivs argMap ++ ")"
        scriptMap = M.mapWithKey f actMap
    in unlines $ map snd $ M.toAscList scriptMap

objEquivalence goal goalExprs =
    let f ((VarE v1) := (VarE v2)) m 
            | isGoalVar v1 goal = M.insert v2 v1 m
            | isGoalVar v2 goal = M.insert v1 v2 m
            | otherwise = m
        f _ m = m
    in foldr f M.empty goalExprs

numArgs dom name = 
    let mbProc = find ((== name) . prcdName) (domProcs dom)
    in case mbProc of
         Just p -> length (prcdArgs p)
         Nothing -> error $ "no proc " ++ name ++ " found"

isGoalVar v goal = v `elem` (map declName (vars goal))

reconstrArgs :: Int -> Integer -> M.Map String String -> M.Map Integer (M.Map Integer ExpY) -> String
reconstrArgs n idx equivs = 
    let exprEquiv (VarE s) = maybe s id (M.lookup s equivs)
        exprEquiv e = show e
    in intercalate "," . take n . map (exprEquiv . snd) . M.toAscList . (M.! idx)

inspectArg (e1 := e2) argsMap = 
    let 
        argTuple = argM e1 <|> argM e2
        val = valM e1 <|> valM e2
        insertArg (name, argNum, index) v =
            M.insertWith M.union index (M.singleton argNum v) argsMap
        argsMapM = insertArg <$> argTuple <*> val
    in maybe argsMap id argsMapM

valM e = case argM e of
           Just _ -> Nothing
           _ -> Just e

argStr = "_arg"
argM (APP (VarE str) [LitI argNum, LitI index]) = 
    (,argNum,index) <$> stripSuffix argStr str
argM _ = Nothing


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