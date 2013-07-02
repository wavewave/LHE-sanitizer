{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
             ScopedTypeVariables, OverloadedStrings, NoMonomorphismRestriction #-}


-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Sanitizer
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Main routine for parsing and processing (sanitizing) LHE file. 
--
-----------------------------------------------------------------------------


module HEP.Parser.LHE.Sanitizer where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Digest.Pure.MD5
import           System.FilePath
import           System.Directory
import           System.IO 
import           System.Random
-- from this package
import           HEP.Parser.LHE.Sanitizer.Action
import           HEP.Parser.LHE.Sanitizer.Type
-- 
import Prelude hiding (dropWhile,takeWhile,sequence)

withRandomTempFile :: (FilePath -> IO ()) -> IO () 
withRandomTempFile act = do 
  tdir <- getTemporaryDirectory 
  r <- randomIO :: IO Int 
  let tmpfile = (tdir </>) .  show . md5 . B.pack . show $ r
  act tmpfile 
  removeFile tmpfile 


-- | sanitize LHE file for one particular order
sanitize1 :: SanitizeJob -> FilePath -> FilePath -> IO () 
sanitize1 (Elim pids) ifp ofp = eliminate pids ifp ofp 
sanitize1 (Replace rpidtable) ifp ofp = replace rpidtable ifp ofp
sanitize1 Shuffle ifp ofp = shuffle ifp ofp 
sanitize1 Blobize ifp ofp = blobize ifp ofp


-- | sanitize LHE file for a given order of sanitizing steps 
sanitize :: [SanitizeJob] -> FilePath -> FilePath -> IO ()
sanitize [] _ _ = return ()
sanitize (x:[]) ifp ofp  = sanitize1 x ifp ofp
sanitize (x:xs) ifp ofp = withRandomTempFile $ \tmp -> sanitize1 x ifp tmp >> sanitize xs tmp ofp  

{- 
sanitizeLHEFile (ElimShuffle pids) ifp ofp = 
  withRandomTempFile $ \tmpfile -> do 
    eliminate pids ifp tmpfile 
    shuffle tmpfile ofp 
sanitizeLHEFile (ReplaceShuffle rpidtable) ifp ofp = 
  withRandomTempFile $ \tmpfile -> do 
    replace rpidtable ifp tmpfile 
    shuffle tmpfile ofp   
-}
 

