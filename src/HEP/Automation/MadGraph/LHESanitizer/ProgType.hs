{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.MadGraph.LHESanitizer.ProgType where 

import System.Console.CmdArgs
import System.FilePath

data LHESanitizer = Convert { lhefilename :: FilePath
                          , outfilename :: FilePath }
                | Count { lhefilename :: FilePath }
              deriving (Show,Data,Typeable)

convert :: LHESanitizer
convert = Convert { lhefilename = "" &= typ "LHEFILE" &= argPos 0
                  , outfilename = "" &= typ "OUTFILE" &= argPos 1 }

count :: LHESanitizer
count = Count { lhefilename = "" &= typ "LHEFILE" &= argPos 0 } 

mode = modes [ convert, count ]

