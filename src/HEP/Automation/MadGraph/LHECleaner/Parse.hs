{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
             ScopedTypeVariables, OverloadedStrings, NoMonomorphismRestriction #-}

module HEP.Automation.MadGraph.LHECleaner.Parse where

import Data.Enumerator as E hiding (head,dropWhile,map)
import qualified Data.Enumerator.List as EL

import Control.Monad as M
import Control.Monad.IO.Class
import Control.Monad.State hiding (sequence)
import Text.XML.Enumerator.Parse.Util

import HEP.Parser.LHEParser.Parser.Enumerator

import HEP.Parser.LHEParser.Type 
import HEP.Parser.LHEParser.DecayTop

import System.IO 

import qualified Data.Map as M

import qualified Data.Foldable as F

import Prelude hiding (dropWhile,takeWhile,sequence)

import Data.Enumerator.Util 

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


printIter :: (MonadIO m, Show s) => Iteratee s m () 
printIter = do 
  elm <- EL.head
  case elm of 
    Nothing -> return ()
    Just c -> do 
      liftIO . putStrLn $ show c
      printIter 

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



doBranch :: (MonadIO m) => 
            (s -> (Bool,s')) 
             -> ((s,s') -> IO ()) 
             -> ((s,s') -> IO ()) 
             -> Iteratee s m ()
doBranch criterion taction faction = do 
    elm <- EL.head
    case elm of 
      Nothing -> return ()
      Just c -> do 
        let (b,c') = criterion c 
        if b 
          then liftIO $ taction (c,c')
          else liftIO $ faction (c,c')
        doBranch criterion taction faction 
  
 


-- action2 ::Iteratee [Event] CountIO (Int,()) 

action2 :: (MonadCount m) => Iteratee s m (Int,())
action2 = enumZip countIter countMarkerIter 

action4 :: (MonadCount m, Show s) => Iteratee s m (Int, (), Integer, ())
action4 = enumZip4 countIter countMarkerIter E.length printIter



------------------------------
------------------------------
------------------------------

getDecayTop :: LHEvent -> ([DecayTop PtlIDInfo])
getDecayTop (LHEvent einfo pinfos) = 
  let pmap = M.fromList (Prelude.map (\x->(idee x,x)) pinfos)
      dtops = mkFullDecayTop (mkIntTree pinfos)
      ptlidinfotop = fmap (mkDecayPDGExtTop pmap) dtops 
--      pdgidtop = fmap (mkDecayPDGTop pmap) dtops 
  in  ptlidinfotop
  
-- lheparseaction = 
decayTopEnee :: (Monad m) => Enumeratee (Maybe LHEvent) (Maybe [DecayTop PtlIDInfo]) m a
decayTopEnee = EL.map (fmap getDecayTop)  


findOnShell :: PDGID -> [DecayTop PtlIDInfo] -> [DecayTop PtlIDInfo]  
findOnShell pid lst =
  let findOnShellWkr x acc = 
        case x of 
          Decay (PIDInfo pid' _, _) -> if (pid==pid') then x:acc else acc
          _ -> acc
  in  foldr findOnShellWkr [] lst 

getPtlID :: DecayTop PtlIDInfo -> PtlID
getPtlID (Decay (pidinfo,_)) = ptlid . ptlinfo $ pidinfo 
getPtlID x = error $ "in getPtlID " ++ (show x)


simpleFormatter :: (Show s) => s -> String 
simpleFormatter s = "---------------------\n" ++ show s ++ "\n"

-- extractOnShellParticle :: 

-------------------------------
-------------------------------
-------------------------------

--boolfunc (Just x) = isOnShell 9000006 x
--boolfunc Nothing = False

-- boolfunc = foldMap (isOnShell 9000006) 

boolfunc (Just x) = (not . null . findOnShell 9000006) x
boolfunc Nothing  = False

-- action6 = enumZip3 countIter countMarkerIter (printNIter 100)

-- action7 = enumZip3 countIter countMarkerIter (printSatisfying boolfunc)

-- (fmap (isOnShell 9000006)) ) 


 

parseLHEFile :: FilePath -> IO () 
parseLHEFile fn = do 
  let iter = parseEventIter process
 --     someAction = doNIter 10 (putStrLn . simpleFormatter . fmap (map getPtlID))
      someAction = doNIter 10 (putStrLn . simpleFormatter . fmap (Prelude.length))
      processinside = decayTopEnee =$ E.filter boolfunc =$ someAction

-- printNIter 10 simpleFormatter 
      process = enumZip3 processinside countIter countMarkerIter


  r <- withFile fn ReadMode $ \h -> 
         flip runStateT (0::Int) (parseXmlFile h iter)

 
  putStrLn $ show r 
  return ()


  