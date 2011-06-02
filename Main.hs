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
         (dom, domCmds)   <- generateDomain domainFileName
         (goal, goalCmds) <- generateGoal dom serialFileName
         Sat goalExprs    <- runCommands domCmds goalCmds
         let script = generateScript goal dom goalExprs 
         print script
         return () 

generateGoal dom fileName = do
  serialE <- parseFromFile serialGoal fileName
  case serialE of
    Right s -> (s,) `fmap` writeYices fileName (goalCommands dom s)
    Left e -> error $ show e


putSat (Sat exprs) = putStrLn "Sat" >> putStrLn (unlines $ map show exprs)
putSat (UnSat _) = putStrLn "Unsat"

runCommands :: [CmdY] -> [CmdY] -> IO ResY
runCommands dCmds gCmds = do
  yPipe <- createYicesPipe "/home/scott/local/bin/yices" []
  putStrLn "Running domain"
  runCmdsY' yPipe dCmds
  putStrLn "Running goal"
  runCmdsY' yPipe gCmds
  s <- checkY yPipe
  putSat s
  return s