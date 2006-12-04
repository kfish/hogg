-----------------------------------------------------------------------------
-- |
-- Module      :  ByteFields
-- Copyright   :  (c) Conrad Parker 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  conradp@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for handling byte-aligned fields
-- Twos-Complement handling adapted from Utils by Dominic Steinitz
--
-----------------------------------------------------------------------------

module Codec.Container.Ogg.ByteFields (
  be64At,
  be32At,
  be16At,
  le64At,
  le32At,
  le16At,
  u8At,
  le64Fill,
  le32Fill,
  le16Fill,
  u8Fill
) where

import Data.Bits
import Data.Int (Int64)
import Data.Word

import qualified Data.ByteString.Lazy as L

beNAt :: Integral a => Int64 -> Int64 -> L.ByteString -> a
beNAt len off s = fromTwosComp $ reverse $ L.unpack (L.take len (L.drop off s))

be64At :: Integral a => Int64 -> L.ByteString -> a
be64At = beNAt 8

be32At :: Integral a => Int64 -> L.ByteString -> a
be32At = beNAt 4

be16At :: Integral a => Int64 -> L.ByteString -> a
be16At = beNAt 2

leNAt :: Integral a => Int64 -> Int64 -> L.ByteString -> a
leNAt len off s = fromTwosComp $ L.unpack (L.take len (L.drop off s))

le64At :: Integral a => Int64 -> L.ByteString -> a
le64At = leNAt 8

le32At :: Integral a => Int64 -> L.ByteString -> a
le32At = leNAt 4

le16At :: Integral a => Int64 -> L.ByteString -> a
le16At = leNAt 2

u8At :: Integral a => Int64 -> L.ByteString -> a
u8At = leNAt 1

-- Generate a ByteString containing the given number
leNFill :: Integral a => Int -> a -> L.ByteString
leNFill n x
  | l < n	= L.pack $ reverse ((take (n-l) $ repeat 0x00) ++ i)
  -- | l > n	= L.pack $ reverse (drop (l-n) i)
  | l > n	= error "leNFill too short"
  | otherwise	= L.pack $ reverse i
                  where l = length i
                        i = toTwosComp x

le64Fill :: Integral a => a -> L.ByteString
le64Fill = leNFill 8

le32Fill :: Integral a => a -> L.ByteString
le32Fill = leNFill 4

le16Fill :: Integral a => a -> L.ByteString
le16Fill = leNFill 2

u8Fill :: Integral a => a -> L.ByteString
u8Fill = leNFill 1


-- | Convert to twos complement, unsigned, big endian
toTwosComp :: Integral a => a -> [Word8]
toTwosComp x
   | x < 0     = error "toTwosComp defined for unsigned only"
   | x == 0    = [0x00]
   | otherwise = toBase 256 x
  where
    toBase x = 
       map fromIntegral .
       reverse .
       map (flip mod x) .
       takeWhile (/=0) .
       iterate (flip div x)

-- | Convert from twos complement, unsigned, little endian

fromTwosComp :: Integral a => [Word8] -> a
fromTwosComp [] = 0
fromTwosComp x = fromWord8s 256 x

-- | Take a list of octets (a number expressed in base n) and convert it
--   to a number.

fromWord8s :: (Integral a, Integral b) => a -> [Word8] -> b
fromWord8s n x = 
    fromIntegral $ sum $ zipWith (*) (powersOf n) (map fromIntegral x)
  where
    powersOf n = 1 : (map (*n) (powersOf n))
