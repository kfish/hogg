-----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for coding and decoding.
--
-----------------------------------------------------------------------------

module Codec.Container.Ogg.Utils (
   -- * Word8 Conversion Functions
   fromTwosComp, toTwosComp
	      ) where

import Data.Word
import Data.Bits

powersOf n = 1 : (map (*n) (powersOf n))

toBase x = 
   map fromIntegral .
   reverse .
   map (flip mod x) .
   takeWhile (/=0) .
   iterate (flip div x)


-- | The most significant bit of a Word8.

msb :: Int
msb = bitSize (undefined::Word8) - 1

-- | Take a list of octets (a number expressed in base n) and convert it
--   to a number.

fromWord8s :: (Integral a, Integral b) => a -> [Word8] -> b
fromWord8s n x = 
   fromIntegral $ 
   sum $ 
   zipWith (*) (powersOf n) (reverse (map fromIntegral x))

-- | Convert from twos complement, unsigned

fromTwosComp :: Integral a => [Word8] -> a
fromTwoComp [] = 0
fromTwosComp x = fromWord8s 256 x

-- | Convert to twos complement, unsigned
toTwosComp :: Integral a => a -> [Word8]
toTwosComp x
   | x < 0     = error "toTwosComp defined for unsigned only"
   | x == 0    = [0x00]
   | otherwise = u
   where z@(y:ys) = toBase 256 (abs x)
         u        = if testBit y msb
                       then 0x00:z
                       else z
