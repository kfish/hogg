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

-- | A section of a chained Ogg physical bitstream. This corresponds to
-- an entire song or video, and most Ogg files in the wild contain only
-- a single chain.
data OggChain =
  OggChain {
    chainTracks :: [OggTrack],
    chainPages :: [OggPage],
    chainPackets :: [OggPacket]
  }
  
-- | Parse a ByteString into a list of OggChains
chainScan :: L.ByteString -> [OggChain]
chainScan d
  | L.null d  = []
  | otherwise = chain : chainScan rest
  where chain = OggChain tracks pages packets
        (tracks, pages) = pageScan d
        packets = pagesToPackets pages
        rest = L.empty

-- | Add a Skeleton logical bitstream to an OggChain
chainAddSkeleton :: OggChain -> OggChain
chainAddSkeleton (OggChain tracks _ packets) = OggChain nt ng np
  where
    nt = [skelTrack] ++ tracks
    ng = packetsToPages np
    np = skelMerge skelPackets packets

    skelTrack = newTrack{trackType = Just Skeleton}
    skelPackets = [fh] ++ indexedFisbones
    fh = fisheadToPacket skelTrack emptyFishead
    fbs = map (fisboneToPacket skelTrack) $ tracksToFisbones tracks
    indexedFisbones = zipWith setPageIx [(length tracks)..] fbs

skelMerge :: [OggPacket] -> [OggPacket] -> [OggPacket]
skelMerge [] ops = ops
skelMerge (fh:fbs) ops = [fh] ++ boss ++ fbs ++ rest
  where (boss, rest) = span packetBOS ops

-- An internal function for setting the pageIx of the segment of a packet.
-- This is only designed for working with packets which are known to only
-- and entirely span one page, such as Skeleton fisbones.
setPageIx :: Int -> OggPacket -> OggPacket
setPageIx ix p@(OggPacket _ _ _ _ _ (Just [oldSegment])) =
  p{packetSegments = Just [newSegment]}
  where
    newSegment = oldSegment{segmentPageIx = ix}
setPageIx _ _ = error "setPageIx used on non-uncut page"
