-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Sanitizer.FileIO
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHE file IO operation
--
-----------------------------------------------------------------------------

module HEP.Parser.LHE.Sanitizer.FileIO where

import Control.Monad.Trans
import Control.Monad.State
import Data.Conduit 
import qualified Data.Text.IO as TIO
import System.IO
import Text.XML.Conduit.Parse.Util
-- 
import Data.Conduit.Util as CU
import Data.Conduit.Util.Count 
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.Type


countEventInLHEFile :: FilePath -> IO ()
countEventInLHEFile fn = 
  withFile fn ReadMode $ \ih -> do 
    let iter = do
          header <- textLHEHeader 
          liftIO $ mapM_ TIO.putStrLn header 
          parseEvent =$ process 
        process = CU.zipSinks countIter countMarkerIter
    r <- flip runStateT (0 :: Int) (parseXmlFile ih iter)
    putStrLn $ show r 

