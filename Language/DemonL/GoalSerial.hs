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

goalAction i = 
  let act = APP (VarE "actions") [exc, LitI i]
      exc = VarE "all-frames"
  in ASSERT act

goalDefs = map typeDefinition . declsToArgsY . vars
  where typeDefinition = flip DEFINE Nothing

goalInitState dom goal = map (assignmentExprs dom (vars goal)) (values goal)

exprYbefore = exprY (LitI 0) (LitI 0)
exprYgoal step = exprY (LitI 0) (LitI step)


assignmentExprs :: DomainU -> [Decl] -> A.Expr -> CmdY
assignmentExprs dom decls = ASSERT . exprYbefore . unsafeCheck decls dom

goalAssert dom goal step = 
    (ASSERT . exprYgoal step . unsafeCheck (vars goal) dom . goalExpr) goal

-- Ich danke für deine Mühen.