{-# LANGUAGE NoMonomorphismRestriction #-}

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

module HEP.Parser.LHE.Sanitizer.Action where

import Control.Monad.State
import Data.Conduit 
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as TIO
import System.IO
import System.Random.Shuffle 
import Text.XML.Conduit.Parse.Util
-- 
import Data.Conduit.Util.Control
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.DecayTop
import HEP.Parser.LHE.Formatter
import HEP.Parser.LHE.Type 
-- 
import HEP.Parser.LHE.Sanitizer.Reconnect
import HEP.Parser.LHE.Sanitizer.Util
--

-- | 
shuffle :: FilePath -> FilePath -> IO () 
shuffle ifn ofn = 
  fileProcInOut ifn ofn $ \(InFile ih) (OutFile oh) -> do 
    (hdr,evs) <- parseXmlFile ih $ do 
                   header <- textLHEHeader
                   rs <- parseEvent =$ CL.consume
                   return (header,rs)
    mapM_ (TIO.hPutStr oh) hdr
    evs' <- shuffleM evs  
    mapM_ (hPrintEv oh) evs'
    hPutStrLn oh "</LesHouchesEvents>\n\n"
 

eliminate :: [Int] -> FilePath -> FilePath -> IO () 
eliminate pids ifn ofn = (fileProcInOut ifn ofn . preserveHeaderAndProcessEvents) $ \h ->
                           doBranchE (checkAndFilterOnShell pids) (onShellAction h) (offShellAction h)


-- | replace 
replace :: [(Int,Int)] -> FilePath -> FilePath -> IO () 
replace pids ifn ofn = (fileProcInOut ifn ofn . preserveHeaderAndProcessEvents) $ \h -> 
                         awaitForever $ liftIO . replaceAction h pids



preserveHeaderAndProcessEvents :: (Handle -> Sink LHEventTop (StateT Int IO) ()) 
                                  -> InFileHandle 
                                  -> OutFileHandle 
                                  ->  IO ()
preserveHeaderAndProcessEvents someAction (InFile ih) (OutFile oh) = do 
  let iter = do 
        header <- textLHEHeader
        liftIO $ mapM_ (TIO.hPutStr oh) $ header 
        parseEvent =$ process
      process = processinside oh
      -- someAction h = doBranchE (checkAndFilterOnShell pids) (onShellAction h) (offShellAction h)
      processinside h = decayTopConduit =$ someAction h
  flip runStateT (0::Int) (parseXmlFile ih iter)
  hPutStrLn oh "</LesHouchesEvents>\n\n"


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



 

      
