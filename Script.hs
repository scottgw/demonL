{-# LANGUAGE TupleSections #-}

module Script where

import Control.Applicative
import Control.Monad

import Data.List
import qualified Data.Map as M

import Math.SMT.Yices.Syntax

generateScript goal dom goalExprs = 
    ( foldr inspectTag M.empty goalExprs
    , foldr inspectArg M.empty goalExprs)

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