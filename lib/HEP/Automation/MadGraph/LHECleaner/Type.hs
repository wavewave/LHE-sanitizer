{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.MadGraph.LHECleaner.Type where 

import System.Console.CmdArgs
import System.FilePath

data LHECleaner = Test { 
                    lhefilename :: FilePath
                  }
              deriving (Show,Data,Typeable)

test :: LHECleaner
test = Test { 
         lhefilename = "" &= typ "LHEFILE" &= argPos 0 
       } 

mode = modes [test]

