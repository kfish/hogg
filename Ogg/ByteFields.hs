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
--
-----------------------------------------------------------------------------

module Ogg.ByteFields (
  le64At,
  le32At,
  le16At,
  u8At
) where

import Data.Word

import Ogg.Utils (fromTwosComp)

leNAt :: Integral a => Int -> Int -> [Word8] -> a
leNAt len off s = fromTwosComp $ reverse (take len (drop off s))

le64At :: Integral a => Int -> [Word8] -> a
le64At = leNAt 8

le32At :: Integral a => Int -> [Word8] -> a
le32At = leNAt 4

le16At :: Integral a => Int -> [Word8] -> a
le16At = leNAt 2

u8At :: Integral a => Int -> [Word8] -> a
u8At = leNAt 1

