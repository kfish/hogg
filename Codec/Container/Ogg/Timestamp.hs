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

import Data.Char
import Data.Maybe
import Data.Ratio
import Text.Printf

import Codec.Container.Ogg.TimeScheme

newtype Timestamp = Timestamp (Maybe (Int, Int))
  deriving Eq

------------------------------------------------------------
-- Ord
--

instance Ord Timestamp where
  compare (Timestamp Nothing) _ = EQ
  compare _ (Timestamp Nothing) = EQ
  compare (Timestamp (Just (n1, d1))) (Timestamp (Just (n2, d2))) =
    compare (n1 % d1) (n2 % d2)

------------------------------------------------------------
-- Show
--

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

------------------------------------------------------------
-- Read
--

instance Read Timestamp where
  readsPrec _ = readsTimestamp

readsTimestamp :: ReadS Timestamp
readsTimestamp str = [(stamp, rest) |
                      (scheme, r) <- reads str :: [(TimeScheme, String)],
                      (time, rest) <- readTime r,
                      stamp <- makeStamp scheme time]

makeStamp :: TimeScheme -> [Int] -> [Timestamp]
makeStamp scheme ts = [Timestamp (Just (n, d))]
  where
     rate = timeSchemeRate scheme
     d = fromIntegral $ numerator rate
     n = (timeSum ts) * (fromIntegral $ numerator rate)

timeSum :: [Int] -> Int
timeSum [] = 0
timeSum [ss] = ss
timeSum [mm, ss] = mm*60 + ss
timeSum [hh, mm, ss] = hh*3600 + mm*60 + ss

readTime :: String -> [([Int], String)]
readTime str = [(nums, rest)]
  where 
        (t, rest) = span (\x -> isAlphaNum x || x == ':') str
        flam = split ':' t
        nums = catMaybes $ map trogdor flam
        trogdor [] = Nothing
        trogdor [a,b] = Just (10 * (digitToInt a) + digitToInt b)

split :: Eq a => a -> [a] -> [[a]]
split delim s
  | rest == [] = [token]
  | otherwise  = token : split delim (tail rest)
  where (token, rest) = span (/= delim) s
