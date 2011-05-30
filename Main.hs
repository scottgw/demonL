module Main where

import Math.SMT.Yices.Pipe

import Text.Parsec.ByteString

import System

import Parser (serialGoal)
import YicesDomain (generateDomain)
import Yices
import GoalSerial

main = do
  args <- getArgs
  if length args < 2 
    then putStrLn "Usage: demonL <domain> <serialization>"
    else 
      do let (domainFileName:serialFileName:_) = args
         domCmds  <- generateDomain domainFileName
         goalCmds <- generateGoal serialFileName
         runCommands domCmds goalCmds

generateGoal fileName = do
  serialE <- parseFromFile serialGoal fileName
  case serialE of
    Right s -> writeYices fileName (goalCommands s)
    Left e -> error $ show e


runCommands dCmds gCmds = do
  yPipe <- createYicesPipe "/home/scott/local/bin/yices" []
  putStrLn "Running domain"
  runCmdsY' yPipe dCmds
  putStrLn "Running goal"
  runCmdsY' yPipe gCmds
  checkY yPipe >>= print