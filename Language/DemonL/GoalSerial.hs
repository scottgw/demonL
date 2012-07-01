module Language.DemonL.GoalSerial where

import Math.SMT.Yices.Syntax

import Language.DemonL.Yices
import qualified Language.DemonL.AST as A
import Language.DemonL.AST (DomainU, Domain (..), Decl (..), declsToMap)
import Language.DemonL.TypeCheck
import Language.DemonL.Goal as G
import Language.DemonL.Types

goalSetup :: DomainU -> SerialGoal -> [CmdY]
goalSetup dom goal = concat [ goalDefs goal
                            , goalInitState dom goal
                            ]

goalAction i = ASSERT $ APP (VarE "actions") [LitI i]

goalDefs = map typeDefinition . declsToArgsY . vars
  where typeDefinition = flip DEFINE Nothing

goalInitState dom goal = map (assignmentExprs dom (vars goal)) (values goal)

exprYbefore = exprY (timeCtx 0) (timeCtx 0)
exprYgoal step = exprY (timeCtx 0) (timeCtx step)


assignmentExprs :: DomainU -> [Decl] -> A.Expr -> CmdY
assignmentExprs dom decls = ASSERT . exprYbefore . unsafeCheck decls dom

goalAssert dom goal step = 
    (ASSERT . exprYgoal step . unsafeCheck (vars goal) dom . goalExpr) goal