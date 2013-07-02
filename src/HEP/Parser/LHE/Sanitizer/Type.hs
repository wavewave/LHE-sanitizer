{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Sanitizer.Type
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- type for sanitizing action
--
-----------------------------------------------------------------------------

module HEP.Parser.LHE.Sanitizer.Type where

import Data.Data

-- | sanitize actions (this is rather temporary) 
data SanitizeType = Elim [Int] 
                  | Replace [(Int,Int)] 
                  | Shuffle 
                  | Blobize
                  | ElimShuffle [Int]
                  | ReplaceShuffle [(Int,Int)]
                  deriving (Show,Typeable,Data)

