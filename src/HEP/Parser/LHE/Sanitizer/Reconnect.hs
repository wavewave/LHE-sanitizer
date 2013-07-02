-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Sanitizer.Reconnect
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- replace routine for sanitizing LHE file.
--
-----------------------------------------------------------------------------

module HEP.Parser.LHE.Sanitizer.Reconnect where

import HEP.Parser.LHE.Type 

import Data.Tuple
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM

-- |
getPtlInfoFromDecayTop :: DecayTop PtlIDInfo -> PtlInfo
getPtlInfoFromDecayTop (Decay (PIDInfo _ pinfo,_)) = pinfo
getPtlInfoFromDecayTop (Terminal (PIDInfo _ pinfo)) = pinfo

-- | 
connDaughterToGrandParents :: DecayTop PtlIDInfo -> PtlInfoMap -> PtlInfoMap 
connDaughterToGrandParents (Decay (PIDInfo _ minfo,ds)) pmap = 
  let dinfos = map getPtlInfoFromDecayTop ds 
      change newmother x = x { mothup = newmother }
      replst = map (\x->(ptlid x,change (mothup minfo) x)) dinfos
      pmap1 = foldr (\x m -> M.update (\_ -> Just (snd x)) (fst x) m) pmap replst 
  in  M.delete (ptlid minfo) pmap1 
connDaughterToGrandParents _ _ = error "connDaughterToGrandParents" 

-- | 
connAllDaughterToGrandParents :: [DecayTop PtlIDInfo] -> PtlInfoMap -> PtlInfoMap
connAllDaughterToGrandParents dtops pmap = foldr (\x m -> connDaughterToGrandParents x m) pmap dtops

-- | 
mkReplaceAssocList :: PtlInfoMap -> ([PtlInfo],[(Int,Int)])
mkReplaceAssocList pmap = let plst = map snd (M.toAscList pmap)
                              f x y = (x,ptlid y)
                          in  (plst, zipWith f [1..] plst)

-- | 
mkReplaceMap :: [(Int,Int)] -> IM.IntMap Int
mkReplaceMap lst = IM.fromList (map swap lst) 

-- | 
applyReplaceMap :: IM.IntMap Int -> PtlInfo -> Maybe PtlInfo 
applyReplaceMap m pinfo = do
  let pid = ptlid pinfo 
      (m1,m2) = mothup pinfo
  pidnew <- IM.lookup pid m 
  m1new <- if m1 == 0 then return 0 else IM.lookup m1 m
  m2new <- if m2 == 0 then return 0 else IM.lookup m2 m
  return pinfo { ptlid = pidnew, mothup = (m1new,m2new) }

-- | 
reSortPtlInfo :: PtlInfoMap -> [PtlInfo]
reSortPtlInfo pmap = 
  let (plst,assoclst) = mkReplaceAssocList pmap
      rmap = mkReplaceMap assoclst 
  in  mapMaybe (applyReplaceMap rmap) plst  

-- |
cleanUpAll :: (LHEvent, PtlInfoMap, [DecayTop PtlIDInfo]) -> [PtlInfo]
cleanUpAll (_,pmap,dtops) = reSortPtlInfo (connAllDaughterToGrandParents dtops pmap) 
                            