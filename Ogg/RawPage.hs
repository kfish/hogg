--
-- Module      : RawPage
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.RawPage (
  OggRawPage (..),
  rawPageScan,
  rawPageBuild
) where

import Ogg.ByteFields

import Data.Word (Word8, Word32, Word64)

import Text.Printf

------------------------------------------------------------
-- The Ogg page format
-- from RFC3533: http://www.ietf.org/rfc/rfc3533.txt
{-

 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1| Byte
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| capture_pattern: Magic number for page start "OggS"           | 0-3
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| version       | header_type   | granule_position              | 4-7
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                               | 8-11
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               | bitstream_serial_number       | 12-15
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               | page_sequence_number          | 16-19
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               | CRC_checksum                  | 20-23
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               |page_segments  | segment_table | 24-27
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| ...                                                           | 28-
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

-}

------------------------------------------------------------
-- Data
--

data OggRawPage =
  OggRawPage {
    rawPageVersion :: Word8,
    rawPageHType :: Word8,
    rawPageGranulepos :: Word64,
    rawPageSerialno :: Word32,
    rawPageSeqno :: Word32,
    rawPageCRC :: Word32,
    rawPageNumseg :: Int,
    rawPageSegtab :: [Int],
    rawPageBody :: [Word8]
  }

------------------------------------------------------------
-- OggPage functions
--

pageMarker :: [Word8]
pageMarker = [0x4f, 0x67, 0x67, 0x53] -- "OggS"

-- | Ogg version supported by this library
pageVersion :: Word8
pageVersion = 0x00

------------------------------------------------------------
-- rawPageScan
--

rawPageScan :: [Word8] -> [OggRawPage]
rawPageScan r@(r1:r2:r3:r4:_)
  | [r1,r2,r3,r4] == pageMarker = newpage : rawPageScan rest
  | otherwise                   = rawPageScan (tail r)
  where (newpage, pageLen) = rawPageBuild r
        rest = drop pageLen r
rawPageScan _ = [] -- length r < 4

rawPageBuild :: [Word8] -> (OggRawPage, Int)
rawPageBuild d = (newRawPage, pageLen) where
  newRawPage = OggRawPage v htype gp serialno seqno crc numseg segtab body
  v = u8At 4 d
  htype = if (length d) > 5 then d !! 5 else 0
  gp = le64At 6 d
  serialno = le32At 14 d
  seqno = le32At 18 d
  crc = le32At 22 d
  numseg = u8At 26 d
  st = take numseg (drop 27 d)
  segtab = map fromIntegral st
  headerSize = 27 + numseg
  bodySize = sum segtab
  body = take bodySize (drop headerSize d)
  pageLen = headerSize + bodySize

------------------------------------------------------------
-- Show
--

instance Show OggRawPage where
  show r =
    " 0                   1                   2                   3\n" ++
    " 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1| Byte\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n" ++
    "| capture_pattern: Magic number for page start \"OggS\"           | 0-3\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n" ++
    "| version       | header_type   | granule_position              | 4-7\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n" ++
    "|                                                               | 8-11\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n" ++
    "|                               | bitstream_serial_number       | 12-15\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n" ++
    "|                               | page_sequence_number          | 16-19\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n" ++
    "|                               | CRC_checksum                  | 20-23\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n" ++
    "|                               |page_segments  | segment_table | 24-27\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n" ++
    "| ...                                                           | 28-\n" ++
    "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n"

