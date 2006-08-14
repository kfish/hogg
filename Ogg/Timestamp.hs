--
-- Module      : Timestamp
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Timestamp (
  Timestamp (..)
) where

import Text.Printf

newtype Timestamp = Timestamp (Maybe (Integer, Integer))
  deriving Eq

instance Show Timestamp where
  show (Timestamp Nothing) = "--:--:--.---"

  show (Timestamp (Just (n, d)))
    | d < 100   = printf "%02d:%02d:%02d::%02d" hrs minN secN framesN
    | otherwise = printf "%02d:%02d:%02d.%03d" hrs minN secN msN
    where
          msN = quot (1000 * framesN) d
          (secT, framesN) = quotRem n d
          (minT, secN) = quotRem secT 60
          (hrs, minN) = quotRem minT 60
