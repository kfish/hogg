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

import Ogg.ByteFields
import Ogg.RawPage
import Ogg.CRC
import Ogg.Utils
import Ogg.Granulepos
import Ogg.Track
import Ogg.Timestamp

import Data.List (find)
import Data.Int (Int64)
import Data.Maybe (maybeToList)
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

-- | Determine the length of a page that would be written
pageLength :: OggPage -> Int
pageLength g = 27 + numsegs + sum (map (fromIntegral . L.length) s)
    where (numsegs, _) = buildSegtab 0 [] incplt s
          incplt = pageIncomplete g
          s = pageSegments g

-- | Calculate the timestamp of a page
pageTimestamp :: OggPage -> Timestamp
pageTimestamp g = timestamp
  where gp = pageGranulepos g
        track = pageTrack g
        timestamp =  gpToTimestamp gp track

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

    version = u8Fill pageVersion
    htype = L.pack [headerType]
    gp_ = le64Fill (gpUnpack gp)
    ser_ = le32Fill serialno
    seqno_ = le32Fill seqno
    crc = le32Fill (genCRC crcPageData)

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
pageScan :: L.ByteString -> ([OggTrack], [OggPage], L.ByteString)
pageScan = pageScan' 0 []

pageScan' :: Int64 -> [OggTrack] -> L.ByteString
          -> ([OggTrack], [OggPage], L.ByteString)
pageScan' offset tracks input
  | L.null input                  = ([], [], L.empty)
  | L.isPrefixOf pageMarker input = pageResult
  | otherwise                     = pageScan' (offset+1) tracks (L.tail input)
  where
        pageResult = pageProcess offset tracks $ pageBuild offset tracks input

pageProcess :: Int64 -> [OggTrack]
            -> Either L.ByteString (OggPage, Int64, L.ByteString, Maybe OggTrack)
            -> ([OggTrack], [OggPage], L.ByteString)
pageProcess _ _ (Left rest) = ([], [], rest)
pageProcess offset tracks (Right (newPage, pageLen, rest, mNewTrack)) =
  (newTrack ++ nextTracks, newPage : nextPages, L.empty)
  where
    (nextTracks, nextPages, _) = pageScan' (offset+pageLen) newTracks rest
    newTrack = maybeToList mNewTrack
    newTracks = newTrack ++ tracks

-- Build an OggPage data structure
pageBuild :: Int64 -> [OggTrack] -> L.ByteString ->
  Either L.ByteString (OggPage, Int64, L.ByteString, Maybe OggTrack)
pageBuild o t d = Right (newPage, pageLen, rest, mNewTrack) where
  newPage = OggPage o track cont incplt bos eos gp seqno segments
  (r, pageLen) = rawPageBuild d
  htype = rawPageHType r
  (mNewTrack, track) = findOrAddTrack serialno body t
  cont = testBit htype 0
  incplt = (not . null) segtab && last segtab == 255
  bos = testBit htype 1
  eos = testBit htype 2
  gp = gpPack (rawPageGranulepos r)
  serialno = rawPageSerialno r
  seqno = rawPageSeqno r
  segtab = rawPageSegtab r
  body = rawPageBody r
  segments = splitSegments 0 segtab body
  rest = L.drop pageLen d 

findOrAddTrack :: Word32 -> L.ByteString -> [OggTrack] -> (Maybe OggTrack, OggTrack)
findOrAddTrack s d t = foat fTrack
  where
    fTrack = find (\x -> trackSerialno x == s) t
    foat :: Maybe OggTrack -> (Maybe OggTrack, OggTrack)
    foat (Just track) = (Nothing, track)
    foat Nothing      = (Just newTrack, newTrack)
    newTrack = bosToTrack s d

-- splitSegments accum segtab body
splitSegments :: Int -> [Int] -> L.ByteString -> [L.ByteString]
splitSegments 0 [0] _ = [L.empty]
splitSegments accum segments body
  | L.null body          = []
  -- accum == 0 &&  L.null segments = []
  | null segments        = [L.take (fromIntegral accum) body]
  | accum == 0 && l == 0 = L.empty : splitSegments 0 ls body
  | l == 255             = splitSegments (accum+255) ls body
  | otherwise            = newseg : splitSegments 0 ls newbody
  where (newseg, newbody) = L.splitAt (fromIntegral (accum+l)) body
        (l:ls) = segments

------------------------------------------------------------
-- Ordering
--

instance Eq OggPage where
  (==) g1 g2 = (==) t1 t2
    where t1 = pageTimestamp g1
          t2 = pageTimestamp g2

instance Ord OggPage where
  compare g1 g2
    | pageBOS g1 = LT
    | pageBOS g2 = GT
    | otherwise = compare t1 t2
    where t1 = pageTimestamp g1
          t2 = pageTimestamp g2

------------------------------------------------------------
-- Show
--

instance Show OggPage where
  show g@(OggPage o track cont incplt bos eos gp _ segment_table) =
    off ++ ": " ++ t ++ " serialno " ++ show (trackSerialno track) ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (pageLength g) ++ " bytes\n" ++ "\t" ++ show (map L.length segment_table) ++ " " ++ show ts ++ "\n" ++ "\n"
    where flags = ifc ++ ift ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ift = if incplt then " (incplt)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
          off = printf "0x%08x" ((fromIntegral o) :: Int)
          ts = pageTimestamp g
          t = maybe "(Unknown)" show (trackType track)
