--
-- Module      : Timestamp
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Timestamp (
  Timestamp (..)
) where

import Data.Ratio
import Text.Printf

newtype Timestamp = Timestamp (Maybe (Integer, Integer))
  deriving Eq

instance Ord Timestamp where
  compare (Timestamp Nothing) _ = EQ
  compare _ (Timestamp Nothing) = EQ
  compare (Timestamp (Just (n1, d1))) (Timestamp (Just (n2, d2))) =
    compare (n1 % d1) (n2 % d2)

instance Show Timestamp where
  show (Timestamp Nothing) = "--:--:--.---"
  show (Timestamp (Just (n, d)))
    | d == 0    = "00:00:00.000"
    | d < 100   = printf "%02d:%02d:%02d::%02d" hrs minN secN framesN
    | otherwise = printf "%02d:%02d:%02d.%03d" hrs minN secN msN
    where
          msN = quot (1000 * framesN) d
          (secT, framesN) = quotRem n d
          (minT, secN) = quotRem secT 60
          (hrs, minN) = quotRem minT 60
