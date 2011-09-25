{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (when)

import Data.Time.Clock

import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Pipe

import Text.Parsec.ByteString

import System.Environment

import Goal
import GoalSerial
import Parser (serialGoal, domain)
import Script (generateScript)
import YicesDomain (procDom)
import Yices

main = do
  args <- getArgs
  if length args < 2 
    then putStrLn "Usage: demonL domain serialization [-d]"
    else 
      do let (domainFileName:goalFileName:others) = args
         let debug = others == ["-d"] 
         eiDom  <- parseFromFile domain domainFileName
         eiGoal <- parseFromFile serialGoal goalFileName
         
         case (eiDom, eiGoal) of
           (Right dom, Right goal) ->
               do dCmds  <- generateDomain dom domainFileName
                  gCmds  <- generateGoal dom goal goalFileName
                  runCommands debug dCmds gCmds dom goal
           e -> error $ "Error during parsing:\n" ++ show e

generateDomain dom fileName = writeYices fileName (procDom dom)
generateGoal dom goal fileName = writeYices fileName (goalSetup dom goal)
 
interpResult (Sat exprs) dom goal =
  putStrLn (unlines $ map show exprs) >> 
  putStrLn (generateScript goal dom exprs)
interpResult (UnSat _) _ _ = putStrLn "Unsat"
interpResult (Unknown _) _ _  = putStrLn "Unknown"
interpResult (InCon ss) _ _ = mapM_ putStrLn ss

-- runCommands :: [CmdY] -> [CmdY] -> IO ResY
runCommands debug dCmds gCmds dom goal = do
  t1 <- getCurrentTime
  yicesPath <- getEnv "YICES_EXE"
  yPipe <- createYicesPipe yicesPath []
  runCmdsY' yPipe dCmds
  runCmdsY' yPipe gCmds
  
  searchUpTo debug yPipe (goalSteps goal) dom goal
  -- searchAll yPipe (goalSteps goal) dom goal
    
  t2 <- getCurrentTime
  when debug (print $ diffUTCTime t1 t1)

searchAll yPipe maxSteps dom goal = do
  runCmdsY' yPipe (map goalAction [0 .. maxSteps - 1])
  runCmdsY' yPipe [goalAssert dom goal maxSteps]
  res <- checkY yPipe
  interpResult res dom goal
  
searchUpTo debug yPipe maxSteps dom goal = 
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
            Sat exprs -> 
              when debug (putStrLn (unlines $ map show exprs)) >> 
              putStrLn (generateScript goal dom exprs)
            Unknown exprs -> 
              putStrLn "Unknown" >>
              when debug (putStrLn (unlines $ map show exprs)) >> 
              putStrLn (generateScript goal dom exprs)
            _          -> pop >> go (i+1)
  in go 0
