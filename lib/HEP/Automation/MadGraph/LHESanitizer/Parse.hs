{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
             ScopedTypeVariables, OverloadedStrings, NoMonomorphismRestriction #-}

module HEP.Automation.MadGraph.LHESanitizer.Parse where

import Control.Monad as M
import Control.Monad.IO.Class
import Control.Monad.State hiding (sequence)

import Data.Conduit as C 
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util as CU
import Data.Conduit.Util.Control
import Data.Conduit.Util.Count
-- import Data.Enumerator.Util 

import HEP.Parser.LHEParser.Parser.Conduit
import HEP.Parser.LHEParser.Type 
import HEP.Parser.LHEParser.DecayTop
import HEP.Parser.LHEParser.Formatter

import HEP.Automation.MadGraph.LHESanitizer.Replace

import Text.XML.Conduit.Parse.Util
import System.IO 
import Prelude hiding (dropWhile,takeWhile,sequence)

import qualified Data.Text.IO as TIO


-- import Data.Enumerator.Util.Count
-- import Data.Enumerator.Util.Control


checkAndFilterOnShell :: [PDGID] 
                      -> (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]) 
                      -> (Bool,(LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]))
checkAndFilterOnShell pids (ev,pmap,dtops) = 
  let dtops' = filterOnShellFromDecayTop pids dtops 
  in  ((not.null) dtops',(ev,pmap,dtops'))


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

offShellAction :: Handle -> (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]) -> IO () 
offShellAction h (ev,_pmap,_dtops) = do
  hPutStrLn h "<event>"
  hPutStrLn h (formatLHEvent ev)
  hPutStrLn h "</event>"

onShellAction :: Handle -> (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]) -> IO ()
onShellAction h (ev,pmap,dtops) = do 
  hPutStrLn h "<event>"
  case ev of 
    LHEvent einfo _ -> do
      let newpinfos = cleanUpAll (ev,pmap,dtops)
          n = Prelude.length newpinfos
      (hPutStrLn h . formatLHEvent) (LHEvent einfo { nup = n }  newpinfos) 
  hPutStrLn h "</event>"

sanitizeLHEFile :: [Int] -> FilePath -> FilePath -> IO () 
sanitizeLHEFile pids ifn ofn = 
  withFile ofn WriteMode $ \oh -> 
    withFile ifn ReadMode $ \ih -> do 
      let iter = do 
            header <- textLHEHeader
            liftIO $ mapM_ (TIO.hPutStr oh) $ header 
            parseEvent =$ process
          process = processinside oh
          someAction h = doBranch (checkAndFilterOnShell pids) (onShellAction h) (offShellAction h)
          processinside h = decayTopConduit =$ someAction h
      flip runStateT (0::Int) (parseXmlFile ih iter)
      hPutStrLn oh "</LesHouchesEvents>\n\n"
      return () 
 
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

