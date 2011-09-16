module HEP.Automation.MadGraph.LHESanitizer.Job where

import HEP.Automation.MadGraph.LHESanitizer.Parse 

startConvert :: Int -> FilePath -> FilePath -> IO () 
startConvert pid ifn ofn = do 
  putStrLn "job started"
  putStrLn $ "processing " ++ ifn ++ " and recording " ++ ofn 
  sanitizeLHEFile pid ifn ofn 

startCount :: FilePath -> IO () 
startCount fn = do 
  putStrLn "job started"
  putStrLn $ "counting events in " ++ fn 
  countEventInLHEFile fn 
