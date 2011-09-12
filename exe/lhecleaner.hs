module Main where

import System.Console.CmdArgs

import HEP.Automation.MadGraph.LHECleaner.Type
import HEP.Automation.MadGraph.LHECleaner.Command

main :: IO () 
main = do 
  putStrLn "LHECleaner"
  param <- cmdArgs mode

  commandLineProcess param