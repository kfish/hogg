--
-- Module      : Track
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Track (
  OggTrack (..),
  nullTrack
) where

import Data.Word (Word32)

------------------------------------------------------------
-- Data
--

data OggTrack =
  OggTrack {
    trackSerialno :: Word32
  }

------------------------------------------------------------
--
--

nullTrack :: OggTrack
nullTrack = OggTrack 0

------------------------------------------------------------
-- Show
--

instance Show OggTrack where
  show (OggTrack serialno) = "serialno " ++ show serialno
