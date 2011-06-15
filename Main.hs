{-# LANGUAGE TupleSections #-}
module Main where

import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Pipe

import Text.Parsec.ByteString

import System

import GoalSerial
import Parser (serialGoal, domain)
import Script (generateScript)
import YicesDomain (procDom)
import Yices

main = do
  args <- getArgs
  if length args < 2 
    then putStrLn "Usage: demonL <domain> <serialization>"
    else 
      do let (domainFileName:goalFileName:_) = args
         eiDom  <- parseFromFile domain domainFileName
         eiGoal <- parseFromFile serialGoal goalFileName
         case (eiDom, eiGoal) of
           (Right dom, Right goal) ->
               do dCmds  <- generateDomain dom domainFileName
                  gCmds  <- generateGoal dom goal goalFileName
                  resY   <- runCommands dCmds gCmds
                  interpResult resY dom goal


generateDomain dom fileName = writeYices fileName (procDom dom)
generateGoal dom goal fileName = writeYices fileName (goalCommands dom goal)
 
interpResult (Sat exprs) dom goal =
  putStrLn $  generateScript goal dom exprs
interpResult (UnSat _) _ _ = putStrLn "Unsat"
interpResult (Unknown _) _ _  = putStrLn "Unknown"
interpResult (InCon ss) _ _ = mapM_ putStrLn ss

runCommands :: [CmdY] -> [CmdY] -> IO ResY
runCommands dCmds gCmds = do
  yPipe <- createYicesPipe "/Users/scott/local/bin/yices" []
  runCmdsY' yPipe dCmds
  runCmdsY' yPipe gCmds
  checkY yPipe
