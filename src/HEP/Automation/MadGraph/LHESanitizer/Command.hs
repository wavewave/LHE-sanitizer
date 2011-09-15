module HEP.Automation.MadGraph.LHESanitizer.Command where

import HEP.Automation.MadGraph.LHESanitizer.ProgType
import HEP.Automation.MadGraph.LHESanitizer.Job

commandLineProcess :: LHESanitizer -> IO ()
commandLineProcess (Convert ifn ofn) = do 
  putStrLn "convert called"
  startConvert ifn ofn
commandLineProcess (Count fn) = do 
  putStrLn "count called"
  startCount fn
