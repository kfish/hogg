module Ogg.Demux where

import Ogg.Utils

import Data.Word (Word8)
import Data.Bits
import Data.Char (isPrint, isAscii, isAlphaNum, isSpace, chr)

import Numeric (showHex)
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

data OggPacket =
  OggPacket {
    packetData :: [Word8],
    packetSerialno :: Int,
    packetGranulepos :: Granulepos,
    packetBOS :: Bool,
    packetEOS :: Bool
  }

------------------------------------------------------------
-- Granulepos functions
--

instance Show Granulepos where
  show (Granulepos (Nothing)) = "-1"
  show (Granulepos (Just gp)) = show gp

------------------------------------------------------------
-- Dump
--

hexDump :: [Word8] -> String
hexDump = _hexDump ""

_hexDump :: String -> [Word8] -> String
_hexDump s [] = s
_hexDump s d = _hexDump (s++lineDump) rest
    where (line, rest) = splitAt 16 d
          lineDump = (take 8 $ repeat ' ') ++ hexLine ++ "  " ++ ascLine ++ "\n"
          hexList = map hexByte line
          hexLine = hexSpace "" hexList False
          -- hexByte x = if x < 16 then '0':(h x) else h x
          -- h x = showHex x ""
          hexByte x = printf "%02x" ((fromIntegral x)::Int)
          hexSpace s [] _ = s
          hexSpace s (c:cs) True = hexSpace (s++c++" ") cs False
          hexSpace s (c:cs) False = hexSpace (s++c) cs True
          ascLine = concat $ map ascByte chars
          chars = map chr (map fromIntegral line)
          ascByte c
            | not $ isAscii c = "."
            | isAlphaNum c = [c]
            | isSpace c = " "
            | otherwise = "."

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
_pageSplit x o c l [] = l++[(o, c)]
_pageSplit x o c l (r1:r2:r3:r4:r)
    | [r1,r2,r3,r4] == pageMarker = _pageSplit (x+4) x pageMarker (l++[(o, c)]) r
    | otherwise                   = _pageSplit (x+1) o (c++[r1]) l (r2:r3:r4:r)
_pageSplit x o c l r = _pageSplit x o (c++r) l []

pageCount = length . pageSplit

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

------------------------------------------------------------
-- OggPacket functions
--

packetBuild :: Int -> [Word8] -> OggPacket
packetBuild s r = OggPacket r s (Granulepos Nothing) False False

packetConcat :: OggPacket -> OggPacket -> OggPacket
packetConcat (OggPacket r1 s1 g1 b1 e1) (OggPacket r2 s2 g2 b2 e2) =
    OggPacket (r1++r2) s1 g2 b1 e2

pages2packets :: [OggPage] -> [OggPacket]
pages2packets = _pages2packets [] Nothing

_pages2packets :: [OggPacket] -> Maybe OggPacket -> [OggPage] -> [OggPacket]
_pages2packets packets Nothing [] = packets
_pages2packets packets (Just jcarry) [] = packets++[jcarry]

_pages2packets packets carry [g] = packets++s
    where s = prependCarry carry (pageToPackets g)

_pages2packets packets carry (g:gn:gs) =
    if (co && length ps == 1) then
        _pages2packets packets (carryCarry carry newcarry) (gn:gs)
    else
        _pages2packets (packets++s) newcarry (gn:gs)
    where s = prependCarry carry ns
          newcarry = if co then Just (last ps) else Nothing
          ns = if co then init ps else ps
          ps = pageToPackets g
          co = pageContinued gn

pageToPackets :: OggPage -> [OggPacket]
pageToPackets page = setGranulepos p2 (pageGranulepos page)
    where p2 = setEOS p1 (pageEOS page)
          p1 = setBOS p0 (pageBOS page)
          p0 = map (packetBuild (pageSerialno page)) (pageSegments page)

setGranulepos :: [OggPacket] -> Granulepos -> [OggPacket]
setGranulepos [] _ = []
setGranulepos ps gp = (init ps)++[(last ps){packetGranulepos = gp}]

setBOS :: [OggPacket] -> Bool -> [OggPacket]
setBOS [] _ = []
setBOS ps False = ps
setBOS (p:ps) True = p{packetBOS = True}:ps

setEOS :: [OggPacket] -> Bool -> [OggPacket]
setEOS [] _ = []
setEOS ps False = ps
setEOS ps True = (init ps)++[(last ps){packetEOS = True}]

carryCarry :: Maybe OggPacket -> Maybe OggPacket -> Maybe OggPacket
carryCarry Nothing Nothing = Nothing
carryCarry Nothing (Just p) = Just p
carryCarry (Just c) Nothing = Just c
carryCarry (Just c) (Just p) = Just (packetConcat c p)

prependCarry :: Maybe OggPacket -> [OggPacket] -> [OggPacket]
prependCarry Nothing s = s
prependCarry (Just c) [] = [c]
prependCarry (Just c) (s:ss) = (packetConcat c s):ss

instance Show OggPacket where
  show (OggPacket d s gp bos eos) =
    "\n: serialno " ++ show s ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (length d) ++ " bytes\n" ++ hexDump d ++ "\n"
    where flags = ifb ++ ife
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
