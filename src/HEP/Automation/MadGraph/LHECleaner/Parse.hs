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

import HEP.Automation.MadGraph.LHECleaner.Replace

import Text.XML.Enumerator.Parse.Util
import System.IO 
import Prelude hiding (dropWhile,takeWhile,sequence)

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
          Nothing -> return ()
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


offShellAction :: (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]) -> IO () 
offShellAction _ = return () -- putStrLn "offshell"

onShellAction :: IORef Int -> (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]) -> IO ()
onShellAction ref (ev,pmap,dtops) = do 
--  let eraserlist = map getPtlID dtops
  let newpinfos = cleanUpAll (ev,pmap,dtops)

--      pmap' = connAllDaughterToGrandParents dtops pmap
--      (plst,assoclst) = mkReplaceAssocList pmap'
--      rmap = mkReplaceMap assoclst
--  putStrLn $ show (mapMaybe (applyReplaceMap rmap) plst) 
--  putStrLn $ show $ connAllDaughterToGrandParents dtops pmap 
  Prelude.mapM (putStrLn . show) newpinfos 
  putStrLn "----------------------"
  st <- readIORef ref
  st `seq` writeIORef ref (st+1)

-- putStrLn "onShell"
 
-- extractOnShellParticle :: 

-------------------------------
-------------------------------
-------------------------------

--boolfunc (Just x) = isOnShell 9000006 x
--boolfunc Nothing = False

-- boolfunc = foldMap (isOnShell 9000006) 



-- boolAndOnShellDecay Nothing  = Nothing 


-- action6 = enumZip3 countIter countMarkerIter (printNIter 100)

-- action7 = enumZip3 countIter countMarkerIter (printSatisfying boolfunc)

-- (fmap (isOnShell 9000006)) ) 



parseLHEFile :: FilePath -> IO () 
parseLHEFile fn = do 
  ref <- newIORef 0
  let iter = parseEventIter (takeEnee 100 =$ process)
      process = enumZip3 processinside countIter countMarkerIter
      someAction = doBranch (checkAndFilterOnShell 9000006) (onShellAction ref) offShellAction
      processinside = decayTopEnee =$ someAction
  r <- withFile fn ReadMode $ \h -> 
         flip runStateT (0::Int) (parseXmlFile h iter)
  putStrLn $ show r
  result <- readIORef ref 
  putStrLn $ show result
  return ()

 --     someAction = doNIter 10 (putStrLn . simpleFormatter . fmap (map getPtlID))
--      someAction = doNIter 10 (putStrLn . simpleFormatter . fmap (Prelude.length))

-- printNIter 10 simpleFormatter 


--      process = countIter
  