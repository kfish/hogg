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

data ParsedTimeStamp =
  ParsedTimeStamp {
    hours :: Int
    , minutes :: Int
    , seconds :: Int
    , subseconds :: Either Int Int -- Left ms or Right frames
  }

instance Read Timestamp where
  readsPrec _ = readsTimestamp

readsTimestamp :: ReadS Timestamp
readsTimestamp str = [(stamp, rest) |
                      (scheme, r) <- reads str :: [(TimeScheme, String)],
                      (time, rest) <- readTime $ tail r,
                      stamp <- makeStamp scheme time]

makeStamp :: TimeScheme -> ParsedTimeStamp -> [Timestamp]
makeStamp scheme ts = map rToTs (timeSum rate ts)
  where
    rate = timeSchemeRate scheme
    rToTs x = Timestamp (Just x)

timeSum :: Rational -> ParsedTimeStamp -> [(Int, Int)]
timeSum rate (ParsedTimeStamp hh mm ss subs) = case subs of
    Left ms -> [((t 1000 1 ms), 1000)]
    Right ff -> [((t n d ff), n)]
    _ -> []
  where
      n = fromIntegral $ numerator rate
      d = fromIntegral $ denominator rate
      t tn td z = fromIntegral $ ((hh*60 +mm)*60 +ss)*tn + td*z

readTime :: String -> [(ParsedTimeStamp, String)]
readTime str = maybe [] (\x -> [(x, rest)]) parsed
  where 
        (t, rest) = span (\x -> isAlphaNum x || x == ':' || x == '.') str
        flam = split ':' t
        parsed :: Maybe ParsedTimeStamp
        parsed = case flam of
          [hh, mm, ss, "", ff] -> fromFrames hh mm ss ff
          [mm, ss, "", ff] -> fromFrames "00" mm ss ff
          [ss, "", ff] -> fromFrames "00" "00" ss ff
          ["", ff] -> fromFrames "00" "00" "00" ff
          [hh, mm, ss] -> fromNPT hh mm ss
          [mm, ss] -> fromNPT "00" mm ss
          [ss] -> fromNPT "00" "00" ss
          _ -> Nothing

        fromFrames :: String -> String -> String -> String
                   -> Maybe ParsedTimeStamp
        fromFrames hh mm ss ff = do
          h <- twoDigits hh
          m <- twoDigits mm
          s <- twoDigits ss
          f <- twoDigits ff
          return $ ParsedTimeStamp h m s (Right f)

        fromNPT hh mm ss = do
          h <- twoDigits hh
          m <- twoDigits mm
          (s, ms) <- fDigits ss
          return $ ParsedTimeStamp h m s (Left ms)

        fDigits a = do
          s <- twoDigits a
          let r = (s, 0)
          return r

        twoDigits [a,b] = Just (10 * (digitToInt a) + digitToInt b)
        twoDigits _ = Nothing

split :: Eq a => a -> [a] -> [[a]]
split delim s
  | rest == [] = [token]
  | otherwise  = token : split delim (tail rest)
  where (token, rest) = span (/= delim) s
