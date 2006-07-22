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
  pageLength,
  pageIsType
) where

import Ogg.RawPage
import Ogg.CRC
import Ogg.Utils
import Ogg.Granulepos
import Ogg.Track

import Data.List (find)
import Data.Word (Word8, Word32)
import Data.Bits

import Text.Printf

------------------------------------------------------------
-- Data
--

data OggPage =
  OggPage {
    pageOffset :: Int,
    pageTrack :: OggTrack,
    pageContinued :: Bool,
    pageIncomplete :: Bool,
    pageBOS :: Bool,
    pageEOS :: Bool,
    pageGranulepos :: Granulepos,
    pageSeqno :: Word32,
    pageSegments :: [[Word8]]
  }

------------------------------------------------------------
-- OggPage functions
--

pageMarker :: [Word8]
pageMarker = [0x4f, 0x67, 0x67, 0x53] -- "OggS"

-- | Ogg version supported by this library
pageVersion :: Word8
pageVersion = 0x00

-- | Determine the length of a page that would be written
pageLength :: OggPage -> Int
pageLength g = 27 + numsegs + sum (map length s)
    where (numsegs, _) = buildSegtab 0 [] s
          s = pageSegments g

------------------------------------------------------------
-- Predicates
--

pageIsType :: OggType -> OggPage -> Bool
pageIsType t g = trackIsType t (pageTrack g)

------------------------------------------------------------
-- pageWrite
--

-- | Construct a binary representation of an Ogg page
pageWrite :: OggPage -> [Word8]
pageWrite (OggPage _ track cont _ bos eos gp seqno s) = newPageData
  where
    newPageData = hData ++ crc ++ sData ++ body
    crcPageData = hData ++ zeroCRC ++ sData ++ body
    hData = pageMarker ++ version ++ htype ++ gp_ ++ ser_ ++ seqno_
    sData = segs

    version = fillField pageVersion 1
    htype = [headerType]
    gp_ = fillField (gpUnpack gp) 8
    ser_ = fillField serialno 4
    seqno_ = fillField seqno 4
    crc = fillField (genCRC crcPageData) 4

    headerType :: Word8
    headerType = c .|. b .|. e
    c = if cont then (bit 0 :: Word8) else 0
    b = if bos then (bit 1 :: Word8) else 0
    e = if eos then (bit 2 :: Word8) else 0

    serialno = trackSerialno track

    -- Segment table
    segs = (toTwosComp (numsegs)) ++ segtab
    (numsegs, segtab) = buildSegtab 0 [] s

    -- Body data
    body = concat s

fillField :: Integral a => a -> Int -> [Word8]
fillField x n
  | l < n	= reverse ((take (n-l) $ repeat 0x00) ++ i)
  | l > n	= reverse (drop (l-n) i)
  | otherwise	= reverse i
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

------------------------------------------------------------
-- pageScan
--

-- | Read a list of data bytes into Ogg pages
pageScan :: [Word8] -> [OggPage]
pageScan = _pageScan 0 []

_pageScan :: Int -> [OggTrack] -> [Word8] -> [OggPage]
_pageScan _ _ [] = []
_pageScan o t r@(r1:r2:r3:r4:_)
    | [r1,r2,r3,r4] == pageMarker = newpage : _pageScan (o+pageLen) nt rest
    | otherwise	= _pageScan (o+1) t (tail r)
      where (newpage, pageLen, rest, nt) = pageBuild o t r
_pageScan _ _ _ = [] -- length r < 4

pageBuild :: Int -> [OggTrack] -> [Word8] -> (OggPage, Int, [Word8], [OggTrack])
pageBuild o t d = (newpage, pageLen, rest, nt) where
  newpage = OggPage o track cont incplt bos eos gp seqno segments
  (r, pageLen) = rawPageBuild d
  htype = rawPageHType r
  (nt, track) = findOrAddTrack serialno body t
  cont = testBit htype 0
  incplt = last segtab == 255
  bos = testBit htype 1
  eos = testBit htype 2
  gp = Granulepos (Just (rawPageGranulepos r))
  serialno = rawPageSerialno r
  seqno = rawPageSeqno r
  segtab = rawPageSegtab r
  body = rawPageBody r
  segments = splitSegments 0 segtab body
  rest = drop pageLen d 

findOrAddTrack :: Word32 -> [Word8] -> [OggTrack] -> ([OggTrack], OggTrack)
findOrAddTrack s d t = foat fTrack
  where
    fTrack = find (\x -> trackSerialno x == s) t
    foat :: Maybe OggTrack -> ([OggTrack], OggTrack)
    foat (Just track) = (t, track)
    foat Nothing      = (nt, newTrack)
    newTrack = OggTrack s ctype
    ctype = readCType d
    nt = t++[newTrack]

-- splitSegments segments accum segtab body
splitSegments :: Int -> [Int] -> [Word8] -> [[Word8]]
splitSegments _ _ [] = []
splitSegments 0 [] _ = []
splitSegments accum [] body = [take accum body]
splitSegments 0 (0:ls) body = [] : splitSegments 0 ls body
splitSegments accum (l:ls) body 
  | l == 255	= splitSegments (accum+255) ls body
  | otherwise	= newseg : splitSegments 0 ls newbody
                  where (newseg, newbody) = splitAt (accum+l) body

------------------------------------------------------------
-- Show
--

instance Show OggPage where
  show p@(OggPage o track cont incplt bos eos gp seqno segment_table) =
    (printf "%07x" o) ++ ": " ++ show track ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (pageLength p) ++ " bytes\n" ++ "\t" ++ show (map length segment_table) ++ "\n"
    where flags = ifc ++ ift ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ift = if incplt then " (incplt)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
