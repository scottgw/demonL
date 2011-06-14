module GoalSerial where

import Math.SMT.Yices.Syntax

import Yices
import qualified AST as A
import AST (DomainU, Domain (..), Decl (..), declsToMap)
import TypeCheck
import Goal as G
import Types

goalCommands :: DomainU -> SerialGoal -> [CmdY]
goalCommands dom goal = concat [ goalDefs goal
                               , goalInitState dom goal
                               , map goalAction [0 .. goalSteps goal - 1]
                               , goalAssert (vars goal) dom goal
                               ]

goalAction i = 
  let act = APP (VarE "actions") [tag, exc, LitI i]
      tag = APP (VarE "tag_array") [LitI i]
      exc = VarE "all-frames"
  in ASSERT act

goalDefs = map typeDefinition . declsToArgsY . vars
  where typeDefinition = flip DEFINE Nothing

goalInitState dom goal = 
    concatMap (assignmentExprs (goalSteps goal) dom (vars goal)) (values goal)

unsafeCheckDom decls dom = unsafeCheck decls (domStructs dom)

exprYbefore = exprY (LitI 0) (LitI 0)
exprYgoal goal = exprY (LitI 0) (LitI $ goalSteps goal)

assignmentExprs :: Integer -> DomainU -> [Decl] -> Assignment -> [CmdY]
assignmentExprs steps dom decls (Assignment name vals) = 
  let
    accessEq attr e = A.BinOpExpr (A.RelOp A.Eq NoType) 
                                  (A.Access (A.Var name) attr) 
                                  e
    typedAccess attr e = unsafeCheckDom decls dom (accessEq attr e)
    accessYices = ASSERT . exprYbefore . uncurry typedAccess
  in map accessYices vals

goalAssert decls dom goal = 
    let steps = goalSteps goal
        genAssert = 
            (:[]) . ASSERT . exprYgoal goal . unsafeCheckDom decls dom . goalExpr
    in genAssert goal

-- Ich danke für deine Mühen.