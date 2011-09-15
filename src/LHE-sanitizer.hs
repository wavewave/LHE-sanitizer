module Main where

import System.Console.CmdArgs

import HEP.Automation.MadGraph.LHESanitizer.ProgType
import HEP.Automation.MadGraph.LHESanitizer.Command

main :: IO () 
main = do 
  putStrLn "LHESanitizer"
  param <- cmdArgs mode

  commandLineProcess param