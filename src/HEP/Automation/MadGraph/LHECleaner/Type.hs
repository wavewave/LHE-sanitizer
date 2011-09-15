{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.MadGraph.LHECleaner.Type where 

import System.Console.CmdArgs
import System.FilePath

data LHECleaner = Convert { lhefilename :: FilePath
                          , outfilename :: FilePath }
                | Count { lhefilename :: FilePath }
              deriving (Show,Data,Typeable)

convert :: LHECleaner
convert = Convert { lhefilename = "" &= typ "LHEFILE" &= argPos 0
                  , outfilename = "" &= typ "OUTFILE" &= argPos 1 }

count :: LHECleaner 
count = Count { lhefilename = "" &= typ "LHEFILE" &= argPos 0 } 

mode = modes [ convert, count ]

