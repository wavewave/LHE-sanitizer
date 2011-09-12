module HEP.Automation.MadGraph.LHECleaner.Job where

import HEP.Automation.MadGraph.LHECleaner.Parse 

startJob :: FilePath -> IO () 
startJob fn = do 
  putStrLn "job started"
  putStrLn $ "processing " ++ fn
  parseLHEFile fn 
