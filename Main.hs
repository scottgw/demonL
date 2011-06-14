{-# LANGUAGE TupleSections #-}
module Main where

import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Pipe

import Text.Parsec.ByteString

import System

import Parser (serialGoal)
import YicesDomain (generateDomain)
import Script (generateScript)
import Yices
import GoalSerial

main = do
  args <- getArgs
  if length args < 2 
    then putStrLn "Usage: demonL <domain> <serialization>"
    else 
      do let (domainFileName:serialFileName:_) = args
         (dom, domCmds)    <- generateDomain domainFileName
         (goal, goalCmds)  <- generateGoal dom serialFileName
         resY              <- runCommands domCmds goalCmds
         interpResult resY dom goal
  
generateGoal dom fileName = do
  serialE <- parseFromFile serialGoal fileName
  case serialE of
    Right s -> (s,) `fmap` writeYices fileName (goalCommands dom s)
    Left e -> error $ show e


interpResult (Sat exprs) dom goal = do 
  putStrLn "Sat" 
  putStrLn (unlines $ map show exprs)
  let script = generateScript goal dom exprs 
  putStrLn script
interpResult (UnSat _) _ _ = putStrLn "Unsat"
interpResult (Unknown _) _ _  = putStrLn "Unknown"
interpResult (InCon ss) _ _ = mapM_ putStrLn ss

runCommands :: [CmdY] -> [CmdY] -> IO ResY
runCommands dCmds gCmds = do
  yPipe <- createYicesPipe "/Users/scott/local/bin/yices" []
  putStrLn "Running domain"
  runCmdsY' yPipe dCmds
  putStrLn "Running goal"
  runCmdsY' yPipe gCmds
  checkY yPipe
