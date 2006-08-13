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
import Data.Int (Int64)
import Data.Word (Word8, Word32)
import Data.Bits
import qualified Data.ByteString.Lazy as L

import Text.Printf

------------------------------------------------------------
-- Data
--

data OggPage =
  OggPage {
    pageOffset :: !Int64,
    pageTrack :: !OggTrack,
    pageContinued :: !Bool,
    pageIncomplete :: !Bool,
    pageBOS :: !Bool,
    pageEOS :: !Bool,
    pageGranulepos :: !Granulepos,
    pageSeqno :: !Word32,
    pageSegments :: !([L.ByteString])
  }

------------------------------------------------------------
-- OggPage functions
--

pageMarker :: L.ByteString
pageMarker = L.pack [0x4f, 0x67, 0x67, 0x53] -- "OggS"

-- | Ogg version supported by this library
pageVersion :: Word8
pageVersion = 0x00

-- | Determine the length of a page that would be written
pageLength :: OggPage -> Int
pageLength g = 27 + numsegs + sum (map (fromIntegral . L.length) s)
    where (numsegs, _) = buildSegtab 0 [] incplt s
          incplt = pageIncomplete g
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
pageWrite :: OggPage -> L.ByteString
pageWrite (OggPage _ track cont incplt bos eos gp seqno s) = newPageData
  where
    newPageData = L.concat [hData, crc, sData, body]
    crcPageData = L.concat [hData, zeroCRC, sData, body]
    hData = L.concat [pageMarker, version, htype, gp_, ser_, seqno_]
    sData = segs

    version = fillField pageVersion 1
    htype = L.pack [headerType]
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
    segs = L.pack $ (toTwosComp (numsegs)) ++ segtab
    (numsegs, segtab) = buildSegtab 0 [] incplt s

    -- Body data
    body = L.concat s

fillField :: Integral a => a -> Int -> L.ByteString
fillField x n
  | l < n	= L.pack $ reverse ((take (n-l) $ repeat 0x00) ++ i)
  | l > n	= L.pack $ reverse (drop (l-n) i)
  | otherwise	= L.pack $ reverse i
                  where l = length i
                        i = toTwosComp x

buildSegtab :: Int -> [Word8] -> Bool -> [L.ByteString] -> (Int, [Word8])
buildSegtab numsegs accum _ [] = (numsegs, accum)
buildSegtab numsegs accum incplt (x:xs) = buildSegtab (numsegs+length(tab)) (accum ++ tab) incplt xs where
  (q,r) = quotRem (fromIntegral $ L.length x) 255
  tab = buildTab q r xs incplt

buildTab :: Int -> Int -> [a] -> Bool -> [Word8]
buildTab 0 r _ _ = [fromIntegral r]
-- don't add [0] if the last seg is cont
buildTab q 0 [] True = take q $ repeat (255 :: Word8)
buildTab q r _ _ = ((take q $ repeat (255 :: Word8)) ++ [fromIntegral r])

------------------------------------------------------------
-- pageScan
--

-- | Read a list of data bytes into Ogg pages
pageScan :: L.ByteString -> [OggPage]
pageScan = pageScan' 0 []

pageScan' :: Int64 -> [OggTrack] -> L.ByteString -> [OggPage]
pageScan' offset tracks input
  | L.null input                  = []
  | L.isPrefixOf pageMarker input = newPage : pageScan' (offset+pageLen) newTracks rest
  | otherwise                     = pageScan' (offset+1) tracks (L.tail input)
  where (newPage, pageLen, rest, newTracks) = pageBuild offset tracks input

pageBuild :: Int64 -> [OggTrack] -> L.ByteString -> (OggPage, Int64, L.ByteString, [OggTrack])
pageBuild o t d = (newPage, pageLen, rest, newTracks) where
  newPage = OggPage o track cont incplt bos eos gp seqno segments
  (r, pageLen) = rawPageBuild d
  htype = rawPageHType r
  (newTracks, track) = findOrAddTrack serialno body t
  cont = testBit htype 0
  incplt = (not . null) segtab && last segtab == 255
  bos = testBit htype 1
  eos = testBit htype 2
  gp = Granulepos (Just (rawPageGranulepos r))
  serialno = rawPageSerialno r
  seqno = rawPageSeqno r
  segtab = rawPageSegtab r
  body = rawPageBody r
  segments = splitSegments 0 segtab body
  rest = L.drop pageLen d 

findOrAddTrack :: Word32 -> L.ByteString -> [OggTrack] -> ([OggTrack], OggTrack)
findOrAddTrack s d t = foat fTrack
  where
    fTrack = find (\x -> trackSerialno x == s) t
    foat :: Maybe OggTrack -> ([OggTrack], OggTrack)
    foat (Just track) = (t, track)
    foat Nothing      = (nt, newTrack)
    newTrack = OggTrack s ctype
    ctype = readCType d
    nt = t++[newTrack]

-- splitSegments accum segtab body
splitSegments :: Int -> [Int] -> L.ByteString -> [L.ByteString]
splitSegments 0 [0] _ = [L.empty]
splitSegments accum segments body
  | L.null body          = []
  -- | accum == 0 &&  L.null segments = []
  | null segments        = [L.take (fromIntegral accum) body]
  | accum == 0 && l == 0 = L.empty : splitSegments 0 ls body
  | l == 255             = splitSegments (accum+255) ls body
  | otherwise            = newseg : splitSegments 0 ls newbody
  where (newseg, newbody) = L.splitAt (fromIntegral (accum+l)) body
        (l:ls) = segments

------------------------------------------------------------
-- Show
--

instance Show OggPage where
  show p@(OggPage o track cont incplt bos eos gp seqno segment_table) =
    (show o) ++ ": " ++ show track ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (pageLength p) ++ " bytes\n" ++ "\t" ++ show (map L.length segment_table) ++ "\n"
    where flags = ifc ++ ift ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ift = if incplt then " (incplt)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
