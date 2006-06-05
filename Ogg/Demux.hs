--
-- Module      : Demux
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Demux where

import Ogg.Utils
import Ogg.Dump

import Data.Word (Word8)
import Data.Bits

import Text.Printf

------------------------------------------------------------
-- Types
--

newtype Granulepos = Granulepos (Maybe Int)

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
    pageSerialno :: Int,
    pageSeqno :: Int,
    pageCRC :: Int,
    pageSegments :: [[Word8]]
  }

------------------------------------------------------------
-- Granulepos functions
--

instance Show Granulepos where
  show (Granulepos (Nothing)) = "-1"
  show (Granulepos (Just gp)) = show gp

------------------------------------------------------------
-- OggPage functions
--

pageMarker :: [Word8]
pageMarker = [0x4f, 0x67, 0x67, 0x53] -- "OggS"

pageSplit :: [Word8] -> [(Int, [Word8])]
pageSplit = _pageSplit 0 0 [] []

-- _pageSplit currOffset lastOffset currData accumResult remainingData
_pageSplit :: Int -> Int -> [Word8] -> [(Int, [Word8])] -> [Word8] -> [(Int, [Word8])]
_pageSplit _ _ [] l [] = l
_pageSplit _ o c l [] = l++[(o, c)]
_pageSplit x o c l (r1:r2:r3:r4:r)
    | [r1,r2,r3,r4] == pageMarker = _pageSplit (x+4) x pageMarker (l++[(o, c)]) r
    | otherwise                   = _pageSplit (x+1) o (c++[r1]) l (r2:r3:r4:r)
_pageSplit x o c l r = _pageSplit x o (c++r) l []

ixSeq :: Int -> Int -> [Word8] -> [Word8]
ixSeq off len s = reverse (take len (drop off s))

readPage :: (Int, [Word8]) -> OggPage
readPage (o, d) = OggPage o d cont bos eos gp serialno seqno crc segments where
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
  body = drop (27+numseg) d
  segments = splitSegments [] 0 segtab body

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
  show (OggPage o d cont bos eos gp serialno seqno crc segment_table) =
    (printf "%07x" o) ++ ": serialno " ++ show serialno ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (length d) ++ " bytes\n" ++ "\t" ++ show (map length segment_table) ++ "\n"
    where flags = ifc ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
