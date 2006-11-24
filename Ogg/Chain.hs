--
-- Module      : Ogg.Chain
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Chain (
  OggChain (..),
  chainScan,
  chainAddSkeleton
) where

import qualified Data.ByteString.Lazy as L

import Ogg.Track
import Ogg.Page
import Ogg.Packet
import Ogg.Skeleton

data OggChain =
  OggChain {
    chainTracks :: [OggTrack],
    chainPages :: [OggPage],
    chainPackets :: [OggPacket]
  }
  
chainScan :: L.ByteString -> [OggChain]
chainScan d
  | L.null d  = []
  | otherwise = chain : chainScan rest
  where chain = OggChain tracks pages packets
        (tracks, pages) = pageScan d
        packets = pagesToPackets pages
        rest = L.empty

chainAddSkeleton :: OggChain -> OggChain
chainAddSkeleton (OggChain tracks pages packets) = OggChain nt ng np
  where
    nt = [skelTrack]
    np = skelPackets
    ng = packetsToPages skelPackets

    skelTrack = newTrack
    skelPackets = [fh] ++ fbs
    fh = fisheadToPacket skelTrack emptyFishead
    fbs = map (fisboneToPacket skelTrack) $ tracksToFisbones tracks
