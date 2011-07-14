{-# LANGUAGE TupleSections #-}
module Main where

import Data.Time.Clock

import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Pipe

import Text.Parsec.ByteString

import System

import Goal
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
         putStrLn "Domain parsed"
         eiGoal <- parseFromFile serialGoal goalFileName
         putStrLn "Goal parsed"
         case (eiDom, eiGoal) of
           (Right dom, Right goal) ->
               do dCmds  <- generateDomain dom domainFileName
                  putStrLn "Domain generated"
                  gCmds  <- generateGoal dom goal goalFileName
                  putStrLn "Goal generated"
                  runCommands dCmds gCmds dom goal
           e -> error $ "Error during parsing:\n" ++ show e

generateDomain dom fileName = writeYices fileName (procDom dom)
generateGoal dom goal fileName = writeYices fileName (goalSetup dom goal)
 
interpResult (Sat exprs) dom goal =
  putStrLn $ generateScript goal dom exprs
interpResult (UnSat _) _ _ = putStrLn "Unsat"
interpResult (Unknown _) _ _  = putStrLn "Unknown"
interpResult (InCon ss) _ _ = mapM_ putStrLn ss

-- runCommands :: [CmdY] -> [CmdY] -> IO ResY
runCommands dCmds gCmds dom goal = do
  t1 <- getCurrentTime
  yicesPath <- getEnv "YICES_EXE"
  yPipe <- createYicesPipe yicesPath []
  runCmdsY' yPipe dCmds
  runCmdsY' yPipe gCmds
  
  searchUpTo yPipe (goalSteps goal) dom goal
    
  t2 <- getCurrentTime
  let diff = diffUTCTime t2 t1
  print diff

searchUpTo yPipe maxSteps dom goal = 
  let run1 = runCmdsY' yPipe . (:[])
      push = run1 PUSH
      pop  = run1 POP
      check = checkY yPipe
      go i 
        | i >= maxSteps = putStrLn "Unsatisfiable"
        | otherwise = do
          run1 (goalAction i)
          push
          run1 (goalAssert dom goal (i+1))
          res <- check
          case res of 
            Sat exprs  -> putStrLn (unlines $ map show exprs) >> 
                          putStrLn (generateScript goal dom exprs)
            _          -> pop >> go (i+1)
  in go 0
