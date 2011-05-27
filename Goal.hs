module Goal where

import Data.Map as M

import AST

data Assignment = 
  Assignment 
  {
    assignVar :: String,
    assignValues :: M.Map String Expr
  } deriving Show

data SerialGoal = 
  SerialGoal 
  {
    vars :: [Decl],
    values :: [Assignment],
    goal :: Expr
  } deriving Show
