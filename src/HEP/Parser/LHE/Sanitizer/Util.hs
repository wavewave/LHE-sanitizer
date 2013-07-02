-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Sanitizer.Util
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- shuffling events to randomize LHE file 
--
-----------------------------------------------------------------------------

module HEP.Parser.LHE.Sanitizer.Util where

import System.IO
--
import HEP.Parser.LHE.Formatter
import HEP.Parser.LHE.Type 

newtype InFileHandle = InFile Handle
newtype OutFileHandle = OutFile Handle

hPrintEv :: Handle -> LHEvent -> IO ()
hPrintEv h ev = hPutStrLn h "<event>" >> hPutStrLn h (formatLHEvent ev) >> hPutStrLn h "</event>"

-- | common file process pattern: input file -> proc -> output file
fileProcInOut :: FilePath -> FilePath 
              -> (InFileHandle -> OutFileHandle -> IO ())
              -> IO ()
fileProcInOut ifn ofn action = 
  withFile ofn WriteMode $ \oh ->
    withFile ifn ReadMode $ \ih ->
      action (InFile ih) (OutFile oh)