module Goal where

import AST

data SerialGoal = 
  SerialGoal 
  {
    vars :: [Decl],
    values :: [Expr],
    goalSteps :: Integer,
    goalExpr :: Expr
  } deriving Show
