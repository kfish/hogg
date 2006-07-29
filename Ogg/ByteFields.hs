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

import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L

import Ogg.Utils (fromTwosComp)

leNAt :: Integral a => Int64 -> Int64 -> L.ByteString -> a
leNAt len off s = fromTwosComp $ reverse $ L.unpack (L.take len (L.drop off s))

le64At :: Integral a => Int64 -> L.ByteString -> a
le64At = leNAt 8

le32At :: Integral a => Int64 -> L.ByteString -> a
le32At = leNAt 4

le16At :: Integral a => Int64 -> L.ByteString -> a
le16At = leNAt 2

u8At :: Integral a => Int64 -> L.ByteString -> a
u8At = leNAt 1

