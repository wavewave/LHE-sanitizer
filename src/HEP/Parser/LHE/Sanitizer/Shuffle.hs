-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Sanitizer.Shuffle
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

module HEP.Parser.LHE.Sanitizer.Shuffle where

import Control.Monad.State
import Control.Monad.Trans

import Data.Conduit 
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as TIO
import System.IO
import System.Random.Shuffle 
import Text.XML.Conduit.Parse.Util
-- 
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.Formatter
import HEP.Parser.LHE.Type 


hPrintEv :: Handle -> LHEvent -> IO ()
hPrintEv h ev = hPutStrLn h "<event>" >> hPutStrLn h (formatLHEvent ev) >> hPutStrLn h "</event>"

-- | 
sanitizeLHEFile_shuffle :: FilePath -> FilePath -> IO () 
sanitizeLHEFile_shuffle ifn ofn = do 
  withFile ofn WriteMode $ \oh -> 
    withFile ifn ReadMode $ \ih -> do 
      (hdr,evs) <- parseXmlFile ih $ do 
                     header <- textLHEHeader
                     -- liftIO $ mapM_ (TIO.hPutStr oh) $ header 
                     rs <- parseEvent =$ CL.consume 
                     return (header,rs)
                     -- process = processinside oh
                     -- someAction h = awaitForever $ liftIO . replaceAction h pids
                     -- processinside h = decayTopConduit =$ someAction h  
      mapM_ (TIO.hPutStr oh) hdr
      evs' <- shuffleM evs  
      mapM_ (hPrintEv oh) evs'
      -- hPutStrLn oh (show (length evs')) 
      hPutStrLn oh "</LesHouchesEvents>\n\n"
      return () 

