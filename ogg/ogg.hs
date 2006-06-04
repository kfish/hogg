module Main where

import Ogg.Utils
import qualified Data.ByteString.Lazy as L

import Data.Word (Word8)
import Data.Bits


------------------------------------------------------------
-- Data
--

data OggPage =
  OggPage {
    raw :: [Word8],
    len :: Int,
    cont :: Bool,
    bos :: Bool,
    eos :: Bool,
    gp :: Int,
    serialno :: Int,
    seqno :: Int,
    crc :: Int,
    numseg :: Int,
    segtab :: [Int],
    page_segments :: [[Word8]]
  }

data OggPacket =
  OggPacket {
    packet_data :: [Word8]
  }

------------------------------------------------------------
-- OggPage functions
--

pageMarker :: [Word8]
pageMarker = [0x4f, 0x67, 0x67, 0x53] -- "OggS"

pageSplit :: [Word8] -> [[Word8]]
pageSplit = _pageSplit [] []

_pageSplit :: [Word8] -> [[Word8]] -> [Word8] -> [[Word8]]
_pageSplit [] l [] = l
_pageSplit c l [] = l++[c]
_pageSplit c l (r1:r2:r3:r4:r)
    | [r1,r2,r3,r4] == pageMarker = _pageSplit pageMarker (l++[c]) r
    | otherwise                   = _pageSplit (c++[r1]) l (r2:r3:r4:r)
_pageSplit c l r = _pageSplit (c++r) l []

pageCount = length . pageSplit

ixSeq :: Int -> Int -> [Word8] -> [Word8]
ixSeq off len s = reverse (take len (drop off s))

readPage :: [Word8] -> OggPage
readPage d = OggPage d len cont bos eos gp serialno seqno crc numseg segtab segments where
  len = length d
  htype = if len > 5 then d !! 5 else 0
  cont = testBit htype 0
  bos = testBit htype 1
  eos = testBit htype 2
  gp = fromTwosComp $ ixSeq 6 8 d
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

segments :: OggPage -> [[Word8]]
segments (OggPage _ _ _ _ _ _ _ _ _ _ _ segment_table) = segment_table

isContinued :: OggPage -> Bool
isContinued (OggPage _ _ cont _ _ _ _ _ _ _ _ _) = cont

instance Show OggPage where
  show (OggPage d l cont bos eos gp serialno seqno crc numsegs segtab segment_table) =
    show seqno ++ ": serialno " ++ show serialno ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show l ++ " bytes (" ++ show (length segment_table) ++ "/" ++ show numsegs ++ "):\n" ++ "\t" ++ show segtab ++ " ->\n" ++ "\t" ++ show (map length segment_table) ++ "\n"
    where flags = ifc ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""

------------------------------------------------------------
-- OggPacket functions
--

packetBuild :: [Word8] -> OggPacket
packetBuild r = OggPacket r

pages2packets :: [OggPage] -> [OggPacket]
pages2packets = _pages2packets [] []

_pages2packets :: [OggPacket] -> [Word8] -> [OggPage] -> [OggPacket]
_pages2packets packets [] [] = packets
_pages2packets packets carry [] = packets++[packetBuild carry]

_pages2packets packets carry (g:gn:gs) =
    if (co && length segs == 1) then
        _pages2packets packets (carry++nc) (gn:gs)
    else
        _pages2packets (packets++s) nc (gn:gs)
    where s = map packetBuild (prependCarry carry ns)
          nc = if co then last segs else []
          ns = if co then init segs else segs
          co = isContinued gn
          segs = segments g

_pages2packets packets carry (g:gs) = _pages2packets (packets++s) carry gs
    where s = map packetBuild (prependCarry carry (segments g))

prependCarry :: [Word8] -> [[Word8]] -> [[Word8]]
prependCarry c [] = [c]
prependCarry c (s:ss) = (c++s):ss

instance Show OggPacket where
  show (OggPacket d) = show "Packet length " ++ show (length d) ++ "\n"

------------------------------------------------------------
-- main
--

main :: IO ()
main = do input <- L.getContents
          putStrLn (show (pages2packets (map readPage (pageSplit $ L.unpack input))))

