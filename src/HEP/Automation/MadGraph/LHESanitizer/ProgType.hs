{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.MadGraph.LHESanitizer.ProgType where 

import System.Console.CmdArgs

data LHESanitizer = Convert { ptlid :: Int
                            , lhefilename :: FilePath
                            , outfilename :: FilePath }
                | Count { lhefilename :: FilePath }
              deriving (Show,Data,Typeable)

convert :: LHESanitizer
convert = Convert { ptlid = 9000006 &= typ "PTLID" &= argPos 0
                  , lhefilename = "" &= typ "LHEFILE" &= argPos 1
                  , outfilename = "" &= typ "OUTFILE" &= argPos 2 }

count :: LHESanitizer
count = Count { lhefilename = "" &= typ "LHEFILE" &= argPos 0 } 

mode :: LHESanitizer
mode = modes [ convert, count ]

