module HEP.Automation.MadGraph.LHECleaner.Command where

import HEP.Automation.MadGraph.LHECleaner.Type
import HEP.Automation.MadGraph.LHECleaner.Job

commandLineProcess :: LHECleaner -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
