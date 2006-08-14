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
  be64At,
  be32At,
  be16At,
  le64At,
  le32At,
  le16At,
  u8At
) where

import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L

import Ogg.Utils (fromTwosComp)

beNAt :: Integral a => Int64 -> Int64 -> L.ByteString -> a
beNAt len off s = fromTwosComp $ L.unpack (L.take len (L.drop off s))

be64At :: Integral a => Int64 -> L.ByteString -> a
be64At = beNAt 8

be32At :: Integral a => Int64 -> L.ByteString -> a
be32At = beNAt 4

be16At :: Integral a => Int64 -> L.ByteString -> a
be16At = beNAt 2

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

