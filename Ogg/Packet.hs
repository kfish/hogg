--
-- Module      : Packet
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Packet (
  OggPacket (..),
  packetsToPages,
  pages2packets
) where

import Ogg.Dump
import Ogg.Granulepos
import Ogg.Page

import Data.Word (Word8, Word32)

------------------------------------------------------------
-- Data
--

data OggPacket =
  OggPacket {
    packetData :: [Word8],
    packetTrack :: OggTrack,
    packetGranulepos :: Granulepos,
    packetBOS :: Bool,
    packetEOS :: Bool,
    packetSegments :: Maybe [OggSegment]
  }

data OggSegment =
  OggSegment {
    segmentLength :: Int,
    segmentEndsPage :: Bool -- ^ whether or not the segment ends a page
  }

------------------------------------------------------------
-- packetsToPages
--

packetsToPages :: [OggPacket] -> [OggPage]
packetsToPages = packetsToPages_ [] Nothing 0

packetsToPages_ :: [OggPage] -> Maybe OggPage -> Word32 -> [OggPacket] -> [OggPage]

packetsToPages_ pages Nothing _ [] = pages
packetsToPages_ pages (Just g) _ [] = pages++[g]

packetsToPages_ pages carry seqno (p:ps)
  = packetsToPages_ (pages++newPages) newCarry (seqno+n) ps
  where
    (newPages, newCarry) = segsToPages [] carry False seqno p
    n = fromIntegral (length newPages)

-- | Convert segments of a packet into pages, and maybe a carry page
segsToPages :: [OggPage] -> Maybe OggPage -> Bool -> Word32 -> OggPacket
               -> ([OggPage], Maybe OggPage)

segsToPages pages _ _ _ (OggPacket _ _ _ _ _ Nothing) = (pages, Nothing)
segsToPages pages _ _ _ (OggPacket _ _ _ _ _ (Just [])) = (pages, Nothing)

segsToPages pages carry cont seqno p@(OggPacket _ _ _ _ _ (Just [s]))
  | segmentEndsPage s = (pages++[newPage], Nothing)
  | otherwise         = (pages, Just newPage)
  where
    newPage = appendToCarry carry cont seqno p

segsToPages pages carry cont seqno
            p@(OggPacket d track gp _ eos (Just (s:ss)))
  = segsToPages (pages++[newPage]) Nothing True (seqno+1) dropPacket
  where
    dropPacket = OggPacket rest track gp False eos (Just ss)
    rest = drop (segmentLength s) d
    newPage = appendToCarry carry cont seqno p

-- | Append the first segment of a packet to the carry page
appendToCarry :: Maybe OggPage -> Bool -> Word32 -> OggPacket -> OggPage

appendToCarry Nothing cont seqno (OggPacket d track gp bos eos (Just [_]))
  = OggPage 0 track cont bos eos gp seqno [d]

appendToCarry Nothing cont seqno (OggPacket d track _ bos _ (Just (s:_)))
  = OggPage 0 track cont bos False (Granulepos Nothing) seqno [seg]
  where seg = take (segmentLength s) d

appendToCarry (Just (OggPage o track cont bos _ _ seqno segs)) _ _
              (OggPacket d _ gp _ eos (Just [_]))
  = OggPage o track cont bos eos gp seqno (segs++[d])

appendToCarry (Just (OggPage o track cont bos _ gp seqno segs)) _ _
              (OggPacket d _ _ _ eos (Just (s:_)))
  = OggPage o track cont bos eos gp seqno (segs++[seg])
  where seg = take (segmentLength s) d
        
-- For completeness
appendToCarry Nothing _ _ (OggPacket _ _ _ _ _ Nothing)
  = OggPage 0 (OggTrack 0) False False False (Granulepos Nothing) 0 []
appendToCarry Nothing _ _ (OggPacket _ _ _ _ _ (Just []))
  = OggPage 0 (OggTrack 0) False False False (Granulepos Nothing) 0 []
appendToCarry (Just carry) _ _ (OggPacket _ _ _ _ _ Nothing) = carry
appendToCarry (Just carry) _ _ (OggPacket _ _ _ _ _ (Just [])) = carry

------------------------------------------------------------
-- pages2packets
--

pages2packets :: [OggPage] -> [OggPacket]
pages2packets = _pages2packets [] Nothing

_pages2packets :: [OggPacket] -> Maybe OggPacket -> [OggPage] -> [OggPacket]
_pages2packets packets Nothing [] = packets
_pages2packets packets (Just jcarry) [] = packets++[jcarry]

_pages2packets packets carry [g] = packets++s
    where s = prependCarry carry (pageToPackets g False)

_pages2packets packets carry (g:gn:gs) =
    if (co && length ps == 1) then
        _pages2packets packets (carryCarry carry newcarry) (gn:gs)
    else
        _pages2packets (packets++s) newcarry (gn:gs)
    where s = prependCarry carry ns
          newcarry = if co then Just (last ps) else Nothing
          ns = if co then init ps else ps
          ps = pageToPackets g co
          co = pageContinued gn

pageToPackets :: OggPage -> Bool -> [OggPacket]
pageToPackets page co = setLastSegmentEnds p3
    where p3 = setGranulepos p2 (pageGranulepos page) co
          p2 = setEOS p1 (pageEOS page)
          p1 = setBOS p0 (pageBOS page)
          p0 = map (packetBuild (pageTrack page)) (pageSegments page)

setLastSegmentEnds :: [OggPacket] -> [OggPacket]
setLastSegmentEnds [] = []
setLastSegmentEnds ps = (init ps) ++ [setSegmentEnds (last ps)]

setSegmentEnds :: OggPacket -> OggPacket
setSegmentEnds p@(OggPacket _ _ _ _ _ (Just [s])) =
  p{packetSegments = (Just [s{segmentEndsPage = True}])}
setSegmentEnds p = p

setGranulepos :: [OggPacket] -> Granulepos -> Bool -> [OggPacket]
setGranulepos = setGranulepos_ []

setGranulepos_ :: [OggPacket] -> [OggPacket] -> Granulepos -> Bool -> [OggPacket]
setGranulepos_ rps [] _ _ = rps
setGranulepos_ rps [p] gp False = rps++[p{packetGranulepos = gp}]
setGranulepos_ rps [p] _ True = rps++[p] -- singleton segment, continued
setGranulepos_ rps [p,pl] gp True = rps++[p{packetGranulepos = gp}]++[pl]
setGranulepos_ rps (p:ps) gp co = setGranulepos_ (rps++[p]) ps gp co

-- setGranulepos [] _ _ = []
-- setGranulepos ps gp co = (init ps)++[(last ps){packetGranulepos = gp}]

setBOS :: [OggPacket] -> Bool -> [OggPacket]
setBOS [] _ = []
setBOS ps False = ps
setBOS (p:ps) True = p{packetBOS = True}:ps

setEOS :: [OggPacket] -> Bool -> [OggPacket]
setEOS [] _ = []
setEOS ps False = ps
setEOS ps True = (init ps)++[(last ps){packetEOS = True}]

-- | Build a partial packet given a track and a segment
packetBuild :: OggTrack -> [Word8] -> OggPacket
packetBuild track r = OggPacket r track (Granulepos Nothing) False False (Just [seg])
    where seg = OggSegment l False
          l = length r

-- | Concatenate data of two (partial) packets into one (partial) packet
packetConcat :: OggPacket -> OggPacket -> OggPacket
packetConcat (OggPacket r1 s1 _ b1 _ (Just x1)) (OggPacket r2 _ g2 _ e2 (Just x2)) =
    OggPacket (r1++r2) s1 g2 b1 e2 (Just (x1++x2))

-- If either of the packets have unknown segmentation, ditch all segmentation
packetConcat (OggPacket r1 s1 _ b1 _ _) (OggPacket r2 _ g2 _ e2 _) =
    OggPacket (r1++r2) s1 g2 b1 e2 Nothing

carryCarry :: Maybe OggPacket -> Maybe OggPacket -> Maybe OggPacket
carryCarry Nothing Nothing = Nothing
carryCarry Nothing (Just p) = Just p
carryCarry (Just c) Nothing = Just c
carryCarry (Just c) (Just p) = Just (packetConcat c p)

prependCarry :: Maybe OggPacket -> [OggPacket] -> [OggPacket]
prependCarry Nothing s = s
prependCarry (Just c) [] = [c]
prependCarry (Just c) (s:ss) = (packetConcat c s):ss

------------------------------------------------------------
-- Show
--

instance Show OggPacket where
  show (OggPacket d track gp bos eos _) =
    ": serialno " ++ show (trackSerialno track) ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (length d) ++ " bytes\n" ++ hexDump d
    where flags = ifb ++ ife
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
