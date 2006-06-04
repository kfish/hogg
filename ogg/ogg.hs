module Main where

import Ogg.Utils
import qualified Data.ByteString.Lazy as L

import Data.Word (Word8)
import Data.Bits

------------------------------------------------------------
-- Types
--

newtype Granulepos = Granulepos (Maybe Int)

------------------------------------------------------------
-- Data
--

data OggPage =
  OggPage {
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
    packetGranulepos :: Granulepos
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
readPage d = OggPage d cont bos eos gp serialno seqno crc segments where
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
  show (OggPage d cont bos eos gp serialno seqno crc segment_table) =
    show seqno ++ ": serialno " ++ show serialno ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (length d) ++ " bytes\n" ++ "\t" ++ show (map length segment_table) ++ "\n"
    where flags = ifc ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""

------------------------------------------------------------
-- OggPacket functions
--

packetBuild :: Int -> [Word8] -> OggPacket
packetBuild s r = OggPacket r s (Granulepos Nothing)

packetConcat :: OggPacket -> OggPacket -> OggPacket
packetConcat (OggPacket r1 s1 g1) (OggPacket r2 s2 g2) =
    OggPacket (r1++r2) s1 g2

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
pageToPackets page = setGranulepos packets (pageGranulepos page)
    where packets = map (packetBuild (pageSerialno page)) (pageSegments page)

setGranulepos :: [OggPacket] -> Granulepos -> [OggPacket]
setGranulepos [] _ = []
setGranulepos ps gp = (init ps)++[(last ps){packetGranulepos = gp}]

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
  show (OggPacket d s gp) =
    ": serialno " ++ show s ++ ", granulepos " ++ show gp ++ ": " ++ show (length d) ++ " bytes\n"

------------------------------------------------------------
-- main
--

main :: IO ()
main = do input <- L.getContents
          putStrLn (show (pages2packets (map readPage (pageSplit $ L.unpack input))))

