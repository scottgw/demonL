module Goal where

import AST

data Assignment = 
  Assignment 
  {
    assignVar :: String,
    assignValues ::[(String, Expr)]
  } deriving Show

data SerialGoal = 
  SerialGoal 
  {
    vars :: [Decl],
    values :: [Assignment],
    goalSteps :: Integer,
    goalExpr :: Expr
  } deriving Show
