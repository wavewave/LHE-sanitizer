{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.MadGraph.LHECleaner.Type where 

import System.Console.CmdArgs

data LHECleaner = Test 
              deriving (Show,Data,Typeable)

test :: LHECleaner
test = Test 

mode = modes [test]

