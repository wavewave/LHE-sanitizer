{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
             ScopedTypeVariables, OverloadedStrings, NoMonomorphismRestriction #-}

module HEP.Automation.MadGraph.LHECleaner.Parse where

import Data.Enumerator hiding (head,dropWhile)
import Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Control.Monad as M
import Control.Monad.IO.Class
import Control.Monad.State hiding (sequence)
import Text.XML.Enumerator.Parse.Util

import HEP.Parser.LHEParser.Parser.Enumerator

import HEP.Parser.LHEParser.Type 
import HEP.Parser.LHEParser.DecayTop

import System.IO 

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


printNIter :: (MonadIO m, Show s) => Int -> Iteratee s m () 
printNIter n 
  | n > 0 = do 
    elm <- EL.head
    case elm of 
      Nothing -> return ()
      Just c -> do 
        liftIO . putStrLn $ show c
        printNIter (n-1) 
  | otherwise = return () -- skipIter 
{-      EL.head_ 
      case elm of 
        Nothing -> return ()
        Just c -> do 
          liftIO . putStrLn $ show c
          printNIter (n-1) 

      return () -}





skipIter = do 
  elm <- EL.head
  case elm of 
    Nothing -> return () 
    Just c -> skipIter  

-- action2 ::Iteratee [Event] CountIO (Int,()) 

action2 :: (MonadCount m) => Iteratee s m (Int,())
action2 = enumZip countIter countMarkerIter 

action4 :: (MonadCount m, Show s) => Iteratee s m (Int, (), Integer, ())
action4 = enumZip4 countIter countMarkerIter E.length printIter

action6 = enumZip3 countIter countMarkerIter (printNIter 100)

getDecayTop :: LHEvent -> IntTree
getDecayTop (LHEvent einfo pinfos) = mkIntTree pinfos

  
-- lheparseaction = 
intTreeEnee = EL.map (fmap getDecayTop)  

parseLHEFile :: FilePath -> IO () 
parseLHEFile fn = do 
  let iter = parseEventIter (intTreeEnee =$ action6)
--  let iter = parseEventIter


-- countIter -- countMarkerIter  -- action2 

  test <- withFile fn ReadMode $ \h -> 
            flip runStateT (0::Int) (parseXmlFile h iter)

 
  putStrLn $ show test 
  return ()


  