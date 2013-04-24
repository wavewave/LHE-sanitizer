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

import           Control.Monad as M
import           Control.Monad.IO.Class
import           Control.Monad.State hiding (sequence)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Conduit as C 
import           Data.Digest.Pure.MD5
-- import qualified Data.Conduit.List as CL
-- import qualified Data.List as L 
-- import           Data.Maybe (fromJust)
import qualified Data.Text.IO as TIO
import           System.FilePath
import           System.Directory
import           System.IO 
import           System.Random
import           Text.XML.Conduit.Parse.Util
-- from hep-platform 
-- import qualified Data.Conduit.Util as CU
import           Data.Conduit.Util.Control
-- import           Data.Conduit.Util.Count
import           HEP.Parser.LHE.Conduit
import           HEP.Parser.LHE.Type 
import           HEP.Parser.LHE.DecayTop
import           HEP.Parser.LHE.Formatter
-- from this package
import           HEP.Parser.LHE.Sanitizer.Eliminate
import           HEP.Parser.LHE.Sanitizer.FileIO
import           HEP.Parser.LHE.Sanitizer.Replace
import           HEP.Parser.LHE.Sanitizer.Shuffle
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


-- | sanitizing LHE file according to spec
sanitizeLHEFile :: SanitizeType -> FilePath -> FilePath -> IO () 
sanitizeLHEFile (Elim pids) ifp ofp = sanitizeLHEFile_eliminate pids ifp ofp 
sanitizeLHEFile (Replace rpidtable) ifp ofp = sanitizeLHEFile_replace rpidtable ifp ofp
sanitizeLHEFile Shuffle ifp ofp = sanitizeLHEFile_shuffle ifp ofp 
sanitizeLHEFile (ElimShuffle pids) ifp ofp = 
  withRandomTempFile $ \tmpfile -> do 
    sanitizeLHEFile_eliminate pids ifp tmpfile 
    sanitizeLHEFile_shuffle tmpfile ofp 
sanitizeLHEFile (ReplaceShuffle rpidtable) ifp ofp = 
  withRandomTempFile $ \tmpfile -> do 
    sanitizeLHEFile_replace rpidtable ifp tmpfile 
    sanitizeLHEFile_shuffle tmpfile ofp   

-- | 
checkAndFilterOnShell :: [PDGID] 
                      -> LHEventTop 
                      -> Either LHEventTop LHEventTop 
                         -- ^ left is on-shell, right is off-shell
checkAndFilterOnShell pids (LHEventTop ev pmap dtops) = 
  let dtops' = filterOnShellFromDecayTop pids dtops 
  in if (not.null) dtops'
       then Left (LHEventTop ev pmap dtops')
       else Right (LHEventTop ev pmap dtops')


replacePDGID :: [(PDGID,PDGID)] -> LHEvent -> LHEvent
replacePDGID pidlst ev@(LHEvent einfo pinfos) = 
    let pinfos' = map rf pinfos 
    in LHEvent einfo pinfos'
  where rf x = case lookup (idup x) pidlst of 
                 Nothing -> x
                 Just nid -> x { idup = nid } 


filterOnShellFromDecayTop :: [PDGID] 
                         -> [DecayTop PtlIDInfo] 
                         -> [DecayTop PtlIDInfo]  
filterOnShellFromDecayTop pids lst =
  let worker x acc = 
        case x of 
          Decay (PIDInfo pid' _, _) -> if (pid' `elem` pids) then x:acc else acc
          _ -> acc
  in  foldr worker [] lst 

getPtlID :: DecayTop PtlIDInfo -> PtlID
getPtlID (Decay (pidinfo,_)) = ptlid . ptlinfo $ pidinfo 
getPtlID x = error $ "in getPtlID " ++ (show x)

offShellAction :: Handle -> LHEventTop -> IO () 
offShellAction h (LHEventTop ev _pmap _dtops) = do
  hPutStrLn h "<event>"
  hPutStrLn h (formatLHEvent ev)
  hPutStrLn h "</event>"

onShellAction :: Handle -> LHEventTop -> IO ()
onShellAction h (LHEventTop ev pmap dtops) = do 
  hPutStrLn h "<event>"
  case ev of 
    LHEvent einfo _ -> do
      let newpinfos = cleanUpAll (ev,pmap,dtops)
          n = Prelude.length newpinfos
      (hPutStrLn h . formatLHEvent) (LHEvent einfo { nup = n }  newpinfos) 
  hPutStrLn h "</event>"

replaceAction :: Handle -> [(Int,Int)] -> LHEventTop -> IO ()
replaceAction h pids (LHEventTop ev _pmap _dtops) = do 
  hPutStrLn h "<event>"
  let ev' = replacePDGID pids ev 
  hPutStrLn h (formatLHEvent ev')
  hPutStrLn h "</event>"



sanitizeLHEFile_eliminate :: [Int] -> FilePath -> FilePath -> IO () 
sanitizeLHEFile_eliminate pids ifn ofn = 
  withFile ofn WriteMode $ \oh -> 
    withFile ifn ReadMode $ \ih -> do 
      let iter = do 
            header <- textLHEHeader
            liftIO $ mapM_ (TIO.hPutStr oh) $ header 
            parseEvent =$ process
          process = processinside oh
          someAction h = doBranchE (checkAndFilterOnShell pids) (onShellAction h) (offShellAction h)
          processinside h = decayTopConduit =$ someAction h
      flip runStateT (0::Int) (parseXmlFile ih iter)
      hPutStrLn oh "</LesHouchesEvents>\n\n"
      return ()
 
-- | replace 
sanitizeLHEFile_replace :: [(Int,Int)] -> FilePath -> FilePath -> IO () 
sanitizeLHEFile_replace pids ifn ofn = do 
  withFile ofn WriteMode $ \oh -> 
    withFile ifn ReadMode $ \ih -> do 
      let iter = do 
            header <- textLHEHeader
            liftIO $ mapM_ (TIO.hPutStr oh) $ header 
            parseEvent =$ process
          process = processinside oh
          someAction h = awaitForever $ liftIO . replaceAction h pids
          processinside h = decayTopConduit =$ someAction h  
      flip runStateT (0::Int) (parseXmlFile ih iter)
      hPutStrLn oh "</LesHouchesEvents>\n\n"
      return () 

      

 

