module HEP.Automation.MadGraph.LHECleaner.Job where

import HEP.Automation.MadGraph.LHECleaner.Parse 

startConvert :: FilePath -> FilePath -> IO () 
startConvert ifn ofn = do 
  putStrLn "job started"
  putStrLn $ "processing " ++ ifn ++ " and recording " ++ ofn 
  parseLHEFile ifn ofn 

startCount :: FilePath -> IO () 
startCount fn = do 
  putStrLn "job started"
  putStrLn $ "counting events in " ++ fn 
  countEventInLHEFile fn 
