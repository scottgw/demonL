module Main where

import Math.SMT.Yices.Syntax
import Text.Parsec.ByteString

import System

import Parser (serialGoal)
import YicesDomain
import GoalSerial

main = do
  args <- getArgs
  if length args < 2 
    then putStrLn "Usage: demonL <domain> <serialization>"
    else 
      do let (domainFileName:serialFileName:_) = args
         generateDomain domainFileName
         runSerialGoal serialFileName

runSerialGoal fileName = do
  serialE <- parseFromFile serialGoal fileName
  case serialE of
    Right s -> print (goalCommands s)
    Left e -> print e
