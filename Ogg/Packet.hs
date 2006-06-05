--
-- Module      : Packet
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Packet where

import Ogg.Dump
import Ogg.Granulepos
import Ogg.Page

import Data.Word (Word8)

------------------------------------------------------------
-- Data
--

data OggPacket =
  OggPacket {
    packetData :: [Word8],
    packetSerialno :: Int,
    packetGranulepos :: Granulepos,
    packetBOS :: Bool,
    packetEOS :: Bool
  }

------------------------------------------------------------
-- OggPacket functions
--

packetBuild :: Int -> [Word8] -> OggPacket
packetBuild s r = OggPacket r s (Granulepos Nothing) False False

packetConcat :: OggPacket -> OggPacket -> OggPacket
packetConcat (OggPacket r1 s1 _ b1 _) (OggPacket r2 _ g2 _ e2) =
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
    ": serialno " ++ show s ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show (length d) ++ " bytes\n" ++ hexDump d
    where flags = ifb ++ ife
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
