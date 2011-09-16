module HEP.Automation.MadGraph.LHESanitizer.Command where

import HEP.Automation.MadGraph.LHESanitizer.ProgType
import HEP.Automation.MadGraph.LHESanitizer.Job

commandLineProcess :: LHESanitizer -> IO ()
commandLineProcess (Convert pid ifn ofn) = do 
  putStrLn "convert called"
  startConvert pid ifn ofn
commandLineProcess (Count fn) = do 
  putStrLn "count called"
  startCount fn
