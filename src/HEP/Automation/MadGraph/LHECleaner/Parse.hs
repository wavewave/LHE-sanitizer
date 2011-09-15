{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
             ScopedTypeVariables, OverloadedStrings, NoMonomorphismRestriction #-}

module HEP.Automation.MadGraph.LHECleaner.Parse where

import Control.Monad as M
import Control.Monad.IO.Class
import Control.Monad.State hiding (sequence)

import Data.Enumerator as E hiding (head,dropWhile,map)
import qualified Data.Enumerator.List as EL
import Data.Enumerator.Util 
import qualified Data.Map as M
import Data.IORef

import Data.Maybe

import HEP.Parser.LHEParser.Parser.Enumerator
import HEP.Parser.LHEParser.Type 
import HEP.Parser.LHEParser.DecayTop
import HEP.Parser.LHEParser.Formatter

import HEP.Automation.MadGraph.LHECleaner.Replace

import Text.XML.Enumerator.Parse.Util
import System.IO 
import Prelude hiding (dropWhile,takeWhile,sequence)

import qualified Data.Text.IO as TIO

------------------------------------
-- Counting
------------------------------------

class (Monad m, MonadIO m) => MonadCount m where 
  getCounter :: m Int 
  putCounter :: Int -> m ()

type CountIO = StateT Int IO 

instance MonadCount CountIO where
  getCounter = get 
  putCounter = put

countIter :: (MonadCount m) => Iteratee s m Int
countIter = do 
  st <- lift getCounter 
  x <- EL.head 
  case x of 
    Nothing -> return st
    Just _ -> do
      st `seq` lift $ putCounter (st+1)
      countIter

countMarkerIter :: (MonadCount m) => Iteratee s m ()
countMarkerIter = do 
  st <- lift getCounter
  when (st `mod` 1000 == 0) $
     liftIO . putStrLn $ show st ++ "th event" 
  t <- EL.head
  case t of 
    Nothing -> return ()
    Just _ -> countMarkerIter 

--------------------------------------
-- Printing
--------------------------------------

printIter :: (MonadIO m, Show s) => Iteratee s m () 
printIter = do 
  elm <- EL.head
  case elm of 
    Nothing -> return ()
    Just c -> do 
      liftIO . putStrLn $ show c
      printIter 

printNIter :: (MonadIO m, Show s) => Int -> (s -> String) -> Iteratee s m () 
printNIter n formatter = doNIter n (putStrLn . formatter)

printSatisfying :: (MonadIO m, Show s) => (s->Bool) -> Iteratee s m ()
printSatisfying f = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return () 
    Just c -> do 
      when (f c) $ do 
        liftIO $ putStrLn "------------------"
        liftIO . putStrLn . show $ c
      printSatisfying f  


simpleFormatter :: (Show s) => s -> String 
simpleFormatter s = "---------------------\n" ++ show s ++ "\n"

--------------------------------------
-- Control Structure
--------------------------------------

doNIter :: (MonadIO m) => Int -> (s -> IO ()) -> Iteratee s m () 
doNIter n action
  | n > 0 = do 
    elm <- EL.head
    case elm of 
      Nothing -> return ()
      Just c -> do 
        liftIO $ action c
        doNIter (n-1) action
  | otherwise = return () 

doBranch :: (MonadIO m) => 
            (s -> (Bool,s')) 
             -> (s' -> IO ()) 
             -> (s' -> IO ()) 
             -> Iteratee (Maybe s) m ()
doBranch criterion taction faction = do 
    elm <- EL.head
    case elm of 
      Nothing -> return ()
      Just maybec -> do 
        case maybec of 
          Just c -> do 
            let (b,c') = criterion c 
            if b 
              then liftIO $ taction c'
              else liftIO $ faction c'
          Nothing -> do 
            liftIO $ putStrLn "what?"
            return ()
        doBranch criterion taction faction 
  
---------------------------------
-- stream mapping
--------------------------------- 

takeEnee :: (Monad m) => Int -> Enumeratee s s m a
takeEnee n = checkDone (continue . (step n)) where
  step _ k EOF = yield (Continue k) EOF
  step num k (Chunks xs) = loop num k xs 

  loop num k [] | num > 0 = continue (step num k)
  loop num k (x:xs) | num > 0 = do 
    k (Chunks [x]) >>== 
      checkDoneEx (Chunks xs) (\k' -> loop (num-1) k' xs)
  loop _ k _ | otherwise = yield (Continue k) EOF


-- action2 ::Iteratee [Event] CountIO (Int,()) 

action2 :: (MonadCount m) => Iteratee s m (Int,())
action2 = enumZip countIter countMarkerIter 

action4 :: (MonadCount m, Show s) => Iteratee s m (Int, (), Integer, ())
action4 = enumZip4 countIter countMarkerIter E.length printIter

------------------------------
------------------------------
------------------------------

getDecayTop :: LHEvent -> (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo])
getDecayTop ev@(LHEvent _einfo pinfos) = 
  let pmap = M.fromList (Prelude.map (\x->(idee x,x)) pinfos)
      dtops = mkFullDecayTop (mkIntTree pinfos)
      ptlidinfotop = fmap (mkDecayPDGExtTop pmap) dtops 
--      pdgidtop = fmap (mkDecayPDGTop pmap) dtops 
  in  (ev,pmap,ptlidinfotop)
  
-- lheparseaction = 
decayTopEnee :: (Monad m) => Enumeratee (Maybe LHEvent) (Maybe (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo])) m a
decayTopEnee = EL.map (fmap getDecayTop)  

checkAndFilterOnShell :: PDGID 
                      -> (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]) 
                      -> (Bool,(LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]))
checkAndFilterOnShell pid (ev,pmap,dtops) = 
  let dtops' = filterOnShellFromDecayTop pid dtops 
  in  ((not.null) dtops',(ev,pmap,dtops'))


filterOnShellFromDecayTop :: PDGID 
                         -> [DecayTop PtlIDInfo] 
                         -> [DecayTop PtlIDInfo]  
filterOnShellFromDecayTop pid lst =
  let worker x acc = 
        case x of 
          Decay (PIDInfo pid' _, _) -> if (pid==pid') then x:acc else acc
          _ -> acc
  in  foldr worker [] lst 

getPtlID :: DecayTop PtlIDInfo -> PtlID
getPtlID (Decay (pidinfo,_)) = ptlid . ptlinfo $ pidinfo 
getPtlID x = error $ "in getPtlID " ++ (show x)


offShellAction :: Handle -> (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]) -> IO () 
offShellAction h (ev,pmap,dtops) = do
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



parseLHEFile :: FilePath -> FilePath -> IO () 
parseLHEFile ifn ofn = 
  withFile ofn WriteMode $ \oh -> 
    withFile ifn ReadMode $ \ih -> do 
      let iter = do 
            header <- textLHEHeader
            liftIO $ mapM_ (TIO.hPutStr oh) $ header 
            parseEventIter process -- (takeEnee 100 =$ process)
          process = enumZip3 (processinside oh) countIter countMarkerIter
          someAction oh = doBranch (checkAndFilterOnShell 9000006) (onShellAction oh) (offShellAction oh)
          processinside oh = decayTopEnee =$ someAction oh

      r <- flip runStateT (0::Int) (parseXmlFile ih iter)
      hPutStrLn oh "</LesHouchesEvents>\n\n"

      putStrLn $ show r 
      return () 
 


countEventInLHEFile :: FilePath -> IO ()
countEventInLHEFile fn = 
  withFile fn ReadMode $ \ih -> do 
    let iter = do
          header <- textLHEHeader 
          liftIO $ mapM_ TIO.putStrLn header 
          parseEventIter process 
        process = enumZip countIter countMarkerIter



    r <- flip runStateT (0 :: Int) (parseXmlFile ih iter)
    putStrLn $ show r 




