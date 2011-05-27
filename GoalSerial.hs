module GoalSerial where

import Math.SMT.Yices.Syntax

import Yices
import AST
import Goal as G
import Types

goalCommands :: SerialGoal -> [CmdY]
goalCommands goal = concat [ goalDefs goal
                           , goalInitState goal
                           , goalAssert goal
                           ]

goalDefs = map typeDefinition . declsToArgsY . vars
  where typeDefinition (n,t) = DEFTYP n (Just t)
        
goalInitState = concatMap assignmentExprs . values

assignmentExprs :: Assignment -> [CmdY]
assignmentExprs (Assignment name vals) = 
  let
    accessEq attr e = BinOpExpr (RelOp Eq NoType) (Access (Var name) attr) e
    accessYices = ASSERT . exprY (LitI 0) . uncurry accessEq
  in map accessYices vals

steps = 5

goalAssert = (:[]) . ASSERT . exprY (LitI steps) . goalExpr