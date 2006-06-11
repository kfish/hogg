--
-- Module      : Page
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Page (
  OggPage (..),
  pageScan,
  pageWrite,
  pageTest
) where

import Ogg.Utils
import Ogg.Granulepos

import Data.Word (Word8, Word32)
import Data.Bits

import Text.Printf

------------------------------------------------------------
-- Data
--

data OggPage =
  OggPage {
    pageOffset :: Int,
    pageData :: [Word8],
    pageContinued :: Bool,
    pageBOS :: Bool,
    pageEOS :: Bool,
    pageGranulepos :: Granulepos,
    pageSerialno :: Word32,
    pageSeqno :: Word32,
    pageCRC :: Word32,
    pageSegments :: [[Word8]]

    ,
    pageHeaderLen :: Int,
    pageBodyLen :: Int
  }

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
-- OggPage functions
--

pageMarker :: [Word8]
pageMarker = [0x4f, 0x67, 0x67, 0x53] -- "OggS"

pageVersion :: Word8
pageVersion = 0x00

------------------------------------------------------------
-- pageWrite
--

pageWrite :: OggPage -> ([Word8], Int, Int, [Word8])
pageWrite (OggPage o p cont bos eos gp serialno seqno crc s hl bl) =
  (newPageData, nhl, nbl, segtab)
--pageWrite :: OggPage -> [Word8]
--pageWrite (OggPage o p cont bos eos gp serialno seqno crc s hl bl) =
--  newPageData
  where
    newPageData = hData ++ sData ++ body
    hData = pageMarker ++ version ++ htype ++ gp_ ++ ser_ ++ seqno_ ++ crc_
    sData = segs

    version = fillField pageVersion 1
    htype = [headerType]
    gp_ = fillField (gpUnpack gp) 8
    ser_ = fillField serialno 4
    seqno_ = fillField seqno 4
    crc_ = fillField crc 4
    
    headerType :: Word8
    headerType = c .|. b .|. e
    c = if cont then (bit 0 :: Word8) else 0
    b = if bos then (bit 1 :: Word8) else 0
    e = if eos then (bit 2 :: Word8) else 0

    -- Segment table
    --segs = (toTwosComp (ns :: Word8)) ++ segtab
    segs = (toTwosComp (numsegs)) ++ segtab
    -- ns = fromIntegral numsegs
    (numsegs, segtab) = buildSegtab 0 [] s

    -- nhl = 4 + 1 + 1 + 8 + 4 + 4 + 4 + 1 + numsegs
    nhl = length (hData) + 1 + numsegs

    body = concat s
    nbl = length body

fillField :: Integral a => a -> Int -> [Word8]
fillField x n
  | l < n	= (take (n-l) $ repeat 0) ++ i
  | l > n	= drop (l-n) i
  | otherwise	= i
                  where l = length i
                        i = toTwosComp x

buildSegtab :: Int -> [Word8] -> [[Word8]] -> (Int, [Word8])
buildSegtab numsegs accum [] = (numsegs, accum)
buildSegtab numsegs accum (x:xs) = buildSegtab (numsegs+length(tab)) (accum ++ tab) xs where
  (q,r) = quotRem (length x) 255
  tab = buildTab q r xs

buildTab :: Int -> Int -> [a] -> [Word8]
buildTab 0 r _ = [fromIntegral r]
-- don't add [0] if the last seg is cont
buildTab q 0 [] = take q $ repeat (255 :: Word8)
buildTab q r _ = ((take q $ repeat (255 :: Word8)) ++ [fromIntegral r])

pageTest :: OggPage -> String
pageTest g@(OggPage o p cont bos eos gp serialno seqno crc segment_table hl bl) =
  report where
    --n = pageWrite g
    (n, nhl, nbl, segtab) = pageWrite g

    report = "Cont: " ++ show cont ++ "\tOriginal Length: " ++ show (length  p) ++ lenEq hl bl ++ "\n"
             ++ "\tNew Length: " ++ show (length n) ++ lenEq nhl nbl ++ "\n"
             ++ "\tSegments: " ++ show (map length segment_table) ++ "\n"
             ++ "\tNew Segs: " ++ show segtab ++ " total: " ++ show (t 0 segtab) ++ "\n"
             ++ "\tLength diff: " ++ show (length p - length n) ++ "\n"
    lenEq h b = " (" ++ show h ++ "+" ++ show b ++ " = " ++ show (h+b) ++ ")"

    t :: Int -> [Word8] -> Int
    t x [] = x
    t x (y:ys) = t (x+yi) ys
      where yi = fromIntegral y

------------------------------------------------------------
-- pageScan
--

pageScan :: [Word8] -> [OggPage]
pageScan = _pageScan 0 []

_pageScan :: Int -> [OggPage] -> [Word8] -> [OggPage]
_pageScan _ l [] = l
_pageScan o l r@(r1:r2:r3:r4:_)
    | [r1,r2,r3,r4] == pageMarker = _pageScan (o+pageLen) (l++[newpage]) rest
    | otherwise	= _pageScan (o+1) l (tail r)
      where (newpage, pageLen, rest) = pageBuild o r
_pageScan _ l _ = l -- length r < 4

pageBuild :: Int -> [Word8] -> (OggPage, Int, [Word8])
pageBuild o d = (newpage, pageLen, rest) where
  newpage = OggPage o p cont bos eos gp serialno seqno crc segments headerSize bodySize
  htype = if (length d) > 5 then d !! 5 else 0
  cont = testBit htype 0
  bos = testBit htype 1
  eos = testBit htype 2
  gp = Granulepos (Just (fromTwosComp $ ixSeq 6 8 d))
  serialno = fromTwosComp $ ixSeq 14 4 d
  seqno = fromTwosComp $ ixSeq 18 4 d
  crc = fromTwosComp $ ixSeq 22 4 d
  numseg = fromTwosComp $ ixSeq 26 1 d
  st = take numseg (drop 27 d)
  segtab = map fromIntegral st
  headerSize = 27 + numseg
  bodySize = sum segtab
  body = take bodySize (drop headerSize d)
  segments = splitSegments [] 0 segtab body
  pageLen = headerSize + bodySize
  (p, rest) = splitAt pageLen d 

ixSeq :: Int -> Int -> [Word8] -> [Word8]
ixSeq off len s = reverse (take len (drop off s))

-- splitSegments segments accum segtab body
splitSegments :: [[Word8]] -> Int -> [Int] -> [Word8] -> [[Word8]]
splitSegments segments _ _ [] = segments
splitSegments segments 0 [] _ = segments
splitSegments segments accum [] body = segments++[take accum body]
splitSegments segments 0 (0:ls) body = splitSegments (segments++[]) 0 ls body
splitSegments segments accum (l:ls) body 
  | l == 255	= splitSegments segments (accum+255) ls body
  | otherwise	= splitSegments (segments++[newseg]) 0 ls newbody
                  where (newseg, newbody) = splitAt (accum+l) body

instance Show OggPage where
  show (OggPage o p cont bos eos gp serialno seqno crc segment_table hl bl) =
    (printf "%07x" o) ++ ": serialno " ++ show serialno ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (length p) ++ " bytes\n" ++ "\t" ++ show (map length segment_table) ++ "\n"
    where flags = ifc ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
