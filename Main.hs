{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (when)

import Data.Time.Clock

import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Pipe

import Text.Parsec.ByteString

import System.Environment

import Language.DemonL.Goal
import Language.DemonL.GoalSerial
import Language.DemonL.Parser (serialGoal, domain)
import Language.DemonL.Script (generateScript)
import Language.DemonL.YicesDomain (procDom)
import Language.DemonL.Yices

main = do
  args <- getArgs
  if length args < 2 
    then putStrLn "Usage: demonL domain goal [-d]"
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

interpResult debug (Sat exprs) dom goal =
  putStrLn (unlines $ map show exprs) >> 
  putStrLn (generateScript goal dom exprs)
interpResult _ (UnSat _) _ _ = putStrLn "Unsat"
interpResult debug (Unknown exprs) dom goal = do
  putStrLn "Unknown"
  when debug $ putStrLn (unlines $ map show exprs)
  putStrLn (generateScript goal dom exprs)
interpResult debug (InCon ss) _ _ = mapM_ putStrLn ss

-- runCommands :: [CmdY] -> [CmdY] -> IO ResY
runCommands debug dCmds gCmds dom goal = do
  t1 <- getCurrentTime
  yicesPath <- getEnv "YICES_EXE"
  yPipe <- createYicesPipe yicesPath []
  runCmdsY' yPipe dCmds
  runCmdsY' yPipe gCmds
  
  found <- searchNone yPipe dom goal


    
  if found
    then   searchAll debug yPipe (goalSteps goal) dom goal -- searchUpTo debug yPipe (goalSteps goal) dom goal
    else putStrLn "Interference impossible"
           
  t2 <- getCurrentTime
  when debug (print $ diffUTCTime t1 t1)

searchNone yPipe dom goal = do
  runCmdsY' yPipe [PUSH]
  runCmdsY' yPipe [goalAssert dom goal 1]
  res <- checkY yPipe
  runCmdsY' yPipe [POP]
  
  return $ case res of
    Sat _ -> True
    Unknown _ -> True
    _ -> False

searchAll debug yPipe maxSteps dom goal = do
  runCmdsY' yPipe (map goalAction [0 .. maxSteps - 1])
  runCmdsY' yPipe [goalAssert dom goal maxSteps]
  res <- checkY yPipe
  interpResult debug res dom goal
  
searchUpTo debug yPipe maxSteps dom goal = 
  let run1 = runCmdsY' yPipe . (:[])
      push = run1 PUSH
      pop  = run1 POP
      check = checkY yPipe
      go i 
        | i >= maxSteps = putStrLn "Unsatisfiable"
        | otherwise = do
          run1 (goalAction i)
          when debug (putStrLn $ show $ goalAction i)
          push
          run1 (goalAssert dom goal (i+1))
          res <- check
          case res of 
            Sat exprs -> 
              when debug (putStrLn (unlines $ map show exprs)) >> 
              putStrLn (generateScript goal dom exprs)
            Unknown exprs -> do
              putStrLn "Unknown"
              when debug (putStrLn (unlines $ map show exprs) >>
                          putStrLn (show $ goalAssert dom goal (i+1)))
              putStrLn (generateScript goal dom exprs)
            _          -> pop >> go (i+1)
  in go 0
