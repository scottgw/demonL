module Main where

import Text.Parsec.ByteString

import System

import Parser (serialGoal)
import YicesDomain

main = do
  args <- getArgs
  if length args < 2 
    then putStrLn "demonL <domain> <serialization>"
    else 
      do let (domainFileName:serialFileName:_) = args
         generateDomain domainFileName
         runSerialGoal serialFileName

runSerialGoal fileName = do
  serialE <- parseFromFile serialGoal fileName
  case serialE of
    Right s -> print s
    Left e -> print e
