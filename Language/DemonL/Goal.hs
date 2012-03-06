module Language.DemonL.Goal where

import Language.DemonL.AST

data SerialGoal = 
  SerialGoal 
  {
    vars :: [Decl],
    values :: [Expr],
    goalSteps :: Integer,
    goalExpr :: Expr
  } deriving Show
