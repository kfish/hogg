--
-- Module      : Ogg.Chain
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Chain (
  OggChain (..),
  chainScan
) where

import qualified Data.ByteString.Lazy as L

import Ogg.Track
import Ogg.Page
import Ogg.Packet

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
