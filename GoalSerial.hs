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
                               , map goalAction [0 .. steps - 1]
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
    concatMap (assignmentExprs dom (vars goal)) (values goal)

unsafeCheckDom decls dom = unsafeCheck decls (domStructs dom)

assignmentExprs :: DomainU -> [Decl] -> Assignment -> [CmdY]
assignmentExprs dom decls (Assignment name vals) = 
  let
    accessEq attr e = A.BinOpExpr (A.RelOp A.Eq NoType) 
                                  (A.Access (A.Var name) attr) 
                                  e
    typedAccess attr e = unsafeCheckDom decls dom (accessEq attr e)
    accessYices = ASSERT . exprY (LitI 0) . uncurry typedAccess
  in map accessYices vals

steps = 2

goalAssert decls dom = 
    (:[]) . ASSERT . exprY (LitI steps) . unsafeCheckDom decls dom . goalExpr

-- Ich danke für deine Mühen.