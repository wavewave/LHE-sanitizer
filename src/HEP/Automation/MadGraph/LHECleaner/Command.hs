module HEP.Automation.MadGraph.LHECleaner.Command where

import HEP.Automation.MadGraph.LHECleaner.Type
import HEP.Automation.MadGraph.LHECleaner.Job

commandLineProcess :: LHECleaner -> IO ()
commandLineProcess (Convert ifn ofn) = do 
  putStrLn "convert called"
  startConvert ifn ofn
commandLineProcess (Count fn) = do 
  putStrLn "count called"
  startCount fn
