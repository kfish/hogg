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
  pagesToPackets,
  packetIsType
) where

import Ogg.Dump
import Ogg.Granulepos
import Ogg.Page
import Ogg.Track
import Ogg.Timestamp

import Data.List as List
import Data.Map as Map
import Data.Word (Word32)
import qualified Data.ByteString.Lazy as L

------------------------------------------------------------
-- Data
--

data OggPacket =
  OggPacket {
    packetData :: !(L.ByteString),
    packetTrack :: !OggTrack,
    packetGranulepos :: !Granulepos,
    packetBOS :: !Bool,
    packetEOS :: !Bool,
    packetSegments :: !(Maybe [OggSegment])
  }

data OggSegment =
  OggSegment {
    segmentLength :: !Int,
    segmentPageIx :: !Int, -- ^ page index (NOT seqno) of this segment
    segmentEndsPage :: !Bool -- ^ whether or not the segment ends a page
  }

------------------------------------------------------------
-- Predicates
--

packetIsType :: OggType -> OggPacket -> Bool
packetIsType t p = trackIsType t (packetTrack p)

packetTimestamp :: OggPacket -> Timestamp
packetTimestamp p = timestamp
  -- | gp == Granulepos Nothing = Timestamp Nothing
  -- | otherwise                = timestamp
  where gp = packetGranulepos p
        track = packetTrack p
        timestamp =  gpToTimestamp gp track

------------------------------------------------------------
-- packetsToPages
--

-- A map from track serialno to seqno
type SeqnoMap = Map.Map OggTrack Word32

data CarryPage = CarryPage {
  carryPageIx :: Int,
  carryPagePage :: OggPage
}

instance Eq CarryPage where
  (==) (CarryPage ix1 _) (CarryPage ix2 _) = (==) ix1 ix2

instance Ord CarryPage where
  compare (CarryPage ix1 _) (CarryPage ix2 _) = compare ix1 ix2

type CarryPages = Map.Map OggTrack CarryPage

packetsToPages :: [OggPacket] -> [OggPage]
packetsToPages = packetsToPages_ Map.empty Map.empty 0 []

packetsToPages_ :: CarryPages -> SeqnoMap -> Int -> [CarryPage] -> [OggPacket] -> [OggPage]

packetsToPages_ carry _ _ q [] = List.map carryPagePage (q ++ (elems carry))

packetsToPages_ carry sqMap ix pageQueue (p:ps)
  = newPages ++ packetsToPages_ newCarry newSqMap newIx newQueue ps
  where
    (newIx, newPages, newQueue) = dequeuePages ix [] tmpQueue
    (tmpQueue, newCarry, newSqMap) = segsToPages pageQueue carry False sqMap p

dequeuePages :: Int -> [OggPage] -> [CarryPage] -> (Int, [OggPage], [CarryPage])
dequeuePages ix oldPages [] = (ix, oldPages, [])
dequeuePages ix oldPages oldQueue@((CarryPage qix qg):qs)
  | ix == qix = dequeuePages (ix+1) (oldPages++[qg]) qs
  | otherwise = (ix, oldPages, oldQueue)

-- | Convert segments of a packet into pages, and maybe a carry page
segsToPages :: [CarryPage] -> CarryPages -> Bool -> SeqnoMap -> OggPacket
               -> ([CarryPage], CarryPages, SeqnoMap)

segsToPages pages carry _ sqMap (OggPacket _ _ _ _ _ Nothing) =
  (pages, carry, sqMap)

segsToPages pages carry _ sqMap (OggPacket _ _ _ _ _ (Just [])) =
  (pages, carry, sqMap)

segsToPages pages carry cont sqMap p@(OggPacket _ track _ _ _ (Just [s]))
  | segmentEndsPage s = (newPages, deleteCarry, newSqMap)
  | otherwise         = (pages, replaceCarry, sqMap)
  where
    newPages = List.insert newPage pages
    newPage = appendToCarry carryPage (segmentPageIx s) cont seqno p
    seqno = Map.findWithDefault 0 track sqMap
    newSqMap = Map.insert track (seqno+1) sqMap
    carryPage = Map.lookup track carry
    deleteCarry = Map.delete track carry
    replaceCarry = Map.insert track newPage carry

segsToPages pages carry cont sqMap
            p@(OggPacket d track gp _ eos (Just (s:ss)))
  = segsToPages newPages deleteCarry True newSqMap dropPacket
  where
    newPages = List.insert newPage pages
    dropPacket = OggPacket rest track gp False eos (Just ss)
    rest = L.drop (fromIntegral $ segmentLength s) d
    seqno = Map.findWithDefault 0 track sqMap
    newSqMap = Map.insert track (seqno+1) sqMap
    deleteCarry = Map.delete track carry
    newPage = appendToCarry carryPage (segmentPageIx s) cont seqno p
    carryPage = Map.lookup track carry

-- | Append the first segment of a packet to (maybe) a carry page
appendToCarry :: Maybe CarryPage -> Int -> Bool -> Word32 -> OggPacket -> CarryPage

-- Case of no carry page, packet has only one segment
appendToCarry Nothing ix cont seqno (OggPacket d track gp bos eos (Just [_]))
  = CarryPage ix (OggPage 0 track cont False bos eos gp seqno [d])

-- Case of no carry page, packet has >1 segment
appendToCarry Nothing ix cont seqno (OggPacket d track _ bos _ (Just (s:_)))
  = CarryPage ix (OggPage 0 track cont True bos False (Granulepos Nothing) seqno [seg])
  where
    seg = L.take (fromIntegral $ segmentLength s) d

-- Case of a carry page, packet has only one segment
appendToCarry (Just (CarryPage ix (OggPage o track cont _ bos _ _ seqno segs))) _ _ _
              (OggPacket d _ gp _ eos (Just [_]))
  = CarryPage ix (OggPage o track cont False bos eos gp seqno (segs++[d]))

-- Case of a carry page, packet has >1 segment
appendToCarry (Just (CarryPage ix (OggPage o track cont _ bos _ gp seqno segs))) _ _ _
              (OggPacket d _ _ _ eos (Just (s:_)))
  = CarryPage ix (OggPage o track cont True bos eos gp seqno (segs++[seg]))
  where seg = L.take (fromIntegral $ segmentLength s) d
        
-- For completeness
appendToCarry _ _ _ _ _ = error "appendToCarry{Ogg.Packet}: nothing to append"

------------------------------------------------------------
-- pagesToPackets
--

type CarryPackets = Map.Map OggTrack OggPacket

pagesToPackets :: [OggPage] -> [OggPacket]
pagesToPackets = _pagesToPackets Map.empty 0

_pagesToPackets :: CarryPackets -> Int -> [OggPage] -> [OggPacket]
_pagesToPackets carry _ [] = elems carry
_pagesToPackets carry ix [g] = prependCarry carry (pageToPackets ix g)

_pagesToPackets carry ix (g:gs) =
    if (incplt && length ps == 1) then
        _pagesToPackets (carryCarry carry newcarry) (ix+1) gs
    else
        s ++ _pagesToPackets newcarry (ix+1) gs
    where s = prependCarry carry ns
          newcarry = if incplt then Map.insert track (last ps) carry
                               else Map.delete track carry
          track = pageTrack g
          ns = if incplt then init ps else ps
          ps = pageToPackets ix g
          incplt = pageIncomplete g

-- | Construct (partial) packets from the segments of a page
pageToPackets :: Int -> OggPage -> [OggPacket]
pageToPackets ix page = setLastSegmentEnds p3
    where p3 = setGranulepos p2 (pageGranulepos page) (pageIncomplete page)
          p2 = setEOS p1 (pageEOS page)
          p1 = setBOS p0 (pageBOS page)
          p0 = List.map (packetBuild (pageTrack page) ix) (pageSegments page)

setLastSegmentEnds :: [OggPacket] -> [OggPacket]
setLastSegmentEnds [] = []
setLastSegmentEnds ps = (init ps) ++ [setSegmentEnds (last ps)]

setSegmentEnds :: OggPacket -> OggPacket
setSegmentEnds p@(OggPacket _ _ _ _ _ (Just [s])) =
  p{packetSegments = (Just [s{segmentEndsPage = True}])}
setSegmentEnds p = p

setGranulepos :: [OggPacket] -> Granulepos -> Bool -> [OggPacket]
setGranulepos [] _ _ = []
setGranulepos [p] gp False = [p{packetGranulepos = gp}]
setGranulepos [p] _ True = [p] -- singleton segment, continued
setGranulepos [p,pl] gp True = [p{packetGranulepos = gp}]++[pl]
setGranulepos (p:ps) gp co = [p] ++ setGranulepos ps gp co

setBOS :: [OggPacket] -> Bool -> [OggPacket]
setBOS [] _ = []
setBOS ps False = ps
setBOS (p:ps) True = p{packetBOS = True}:ps

setEOS :: [OggPacket] -> Bool -> [OggPacket]
setEOS [] _ = []
setEOS ps False = ps
setEOS ps True = (init ps)++[(last ps){packetEOS = True}]

-- | Build a partial packet given a track, seqno and a segment
packetBuild :: OggTrack -> Int -> L.ByteString -> OggPacket
packetBuild track ix r = OggPacket r track (Granulepos Nothing) False False (Just [seg])
    where seg = OggSegment (fromIntegral l) ix False
          l = L.length r

-- | Concatenate data of two (partial) packets into one (partial) packet
packetConcat :: OggPacket -> OggPacket -> OggPacket
packetConcat (OggPacket r1 s1 _ b1 _ (Just x1)) (OggPacket r2 _ g2 _ e2 (Just x2)) =
    OggPacket (L.append r1 r2) s1 g2 b1 e2 (Just (x1++x2))

-- If either of the packets have unknown segmentation, ditch all segmentation
packetConcat (OggPacket r1 s1 _ b1 _ _) (OggPacket r2 _ g2 _ e2 _) =
    OggPacket (L.append r1 r2) s1 g2 b1 e2 Nothing

carryCarry :: CarryPackets -> CarryPackets -> CarryPackets
carryCarry oldCarry newCarry
  | Map.null oldCarry = newCarry
  | Map.null newCarry = oldCarry
  | otherwise     = Map.insert track combinedCarry oldCarry
  where (track, p) = (head . Map.assocs) newCarry
        combinedCarry = concatTo $ Map.lookup track oldCarry
        concatTo Nothing = p
        concatTo (Just c) = packetConcat c p

prependCarry :: CarryPackets -> [OggPacket] -> [OggPacket]
prependCarry oldCarry [] = elems oldCarry
prependCarry oldCarry segs@(s:ss) = newPackets
  where track = packetTrack s
        newPackets = appendTo $ Map.lookup track oldCarry
        appendTo Nothing = segs
        appendTo (Just c) = (packetConcat c s):ss

------------------------------------------------------------
-- Show
--

instance Show OggPacket where
  show p@(OggPacket d track gp bos eos _) =
    show ts ++ ": " ++ t ++ " serialno: " ++ show (trackSerialno track) ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (L.length d) ++ " bytes\n" ++ hexDump d ++ "\n"
    where flags = ifb ++ ife
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
          ts = packetTimestamp p
          t = maybe "(Unknown)" show (trackType track)
