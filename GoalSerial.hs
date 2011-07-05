module GoalSerial where

import Math.SMT.Yices.Syntax

import Yices
import qualified AST as A
import AST (DomainU, Domain (..), Decl (..), declsToMap)
import TypeCheck
import Goal as G
import Types

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

unsafeCheckDom decls dom = unsafeCheck decls (domStructs dom)

exprYbefore = exprY (LitI 0) (LitI 0)
exprYgoal step = exprY (LitI 0) (LitI step)


assignmentExprs :: DomainU -> [Decl] -> A.Expr -> CmdY
assignmentExprs dom decls = ASSERT . exprYbefore . unsafeCheckDom decls dom

goalAssert dom goal step = 
    (ASSERT . exprYgoal step . unsafeCheckDom (vars goal) dom . goalExpr) goal

-- Ich danke für deine Mühen.