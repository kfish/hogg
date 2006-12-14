--
-- Module      : Track
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Track (
  OggTrack (..),
  trackIsType,
  newTrack,
  nullTrack,
  bosToTrack,
  gpToTimestamp
) where

import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Word (Word32)
import Data.Ratio

import Text.Printf

import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Granulerate
import Codec.Container.Ogg.MessageHeaders
import Codec.Container.Ogg.Timestamp

------------------------------------------------------------
-- Data
--

data OggTrack =
  OggTrack {
    trackSerialno :: Word32,
    trackType :: Maybe ContentType,
    trackGranulerate :: Maybe Granulerate,
    trackGranuleshift :: Maybe Int,
    trackMetadata :: MessageHeaders
  }

------------------------------------------------------------
-- ContentTyped
--

-- | Predicate
trackIsType :: ContentType -> OggTrack -> Bool
trackIsType t0 track
  | (Just t0) == t1  = True
  | otherwise        = False
  where t1 = trackType track

instance ContentTyped OggTrack where
  contentTypeIs = trackIsType
  contentTypeOf = trackType

------------------------------------------------------------
--
--

-- | The null track
nullTrack :: OggTrack
nullTrack = OggTrack 0 Nothing Nothing Nothing mhEmpty

-- | A new track, with a given serialno
newTrack :: Word32 -> OggTrack
newTrack serialno = OggTrack serialno Nothing Nothing Nothing mhEmpty

-- Instantiate an OggTrack given a serialno and a bos page
bosToTrack :: Word32 -> L.ByteString -> OggTrack
bosToTrack s d = OggTrack s ctype gr gs mh
  where
    ctype = identify d
    gr = maybe Nothing (\x -> granulerate x d) ctype
    gs = maybe Nothing (\x -> granuleshift x d) ctype
    mh = maybe mhEmpty (\x -> metadata x d) ctype

-- | Convert a granulepos to a timestamp
gpToTimestamp :: Granulepos -> OggTrack -> Maybe Timestamp
gpToTimestamp mgp track
  | g == Nothing = Nothing
  | r == Nothing = Nothing
  | otherwise    = Just (Timestamp timestamp)
  where g = gpToGranules mgp track
        r = trackGranulerate track
        timestamp = (granules*d) % n
        n = numerator gr
        d = denominator gr
        Just granules = g
        Just (Granulerate gr) = r

-- | Convert a granluepos to a count of granules
gpToGranules :: Granulepos -> OggTrack -> Maybe Integer
gpToGranules mgp track
  | s == Nothing = Nothing
  | otherwise    = Just (keyframe + delta)
  where s = gpSplit mgp track
        Just (keyframe, delta) = s

-- | Split a granulepos by its track's granuleshift
gpSplit :: Granulepos -> OggTrack -> Maybe (Integer, Integer)
gpSplit mgp track
  | mgp == Granulepos Nothing          = Nothing
  | trackGranuleshift track == Nothing = Just (gp, 0)
  | otherwise                          = Just (keyframe, delta)
  where Granulepos (Just w64gp) = mgp
        Just gShift = trackGranuleshift track
        gp = fromIntegral w64gp
        keyframe = fromIntegral $ w64gp `shiftR` gShift
        delta = fromIntegral $ gp - (keyframe `shiftL` gShift)

-- | Tracks are equal if their serialnos are equal
instance Eq OggTrack where
  (==) track1 track2 = s1 == s2
       where s1 = trackSerialno track1
             s2 = trackSerialno track2

instance Ord OggTrack where
  compare track1 track2 = compare s1 s2
          where s1 = trackSerialno track1
                s2 = trackSerialno track2

------------------------------------------------------------
-- Show
--

instance Show OggTrack where
  -- show (OggTrack serialno ctype gr gs mhdrs) =
  show (OggTrack serialno ctype _ _ mhdrs) =
    t ++ ": serialno " ++ s ++ "\n" ++ m
    where s = printf "%010u" ((fromIntegral serialno) :: Int)
          t = maybe "(Unknown)" show ctype
          m = unlines $ zipWith (++) (repeat "\t") (lines $ show mhdrs)

  -- show (OggTrack serialno _ _) =
  --   "(Unknown): serialno " ++ s ++ "\n"
  --   where s = printf "%010d" ((fromIntegral serialno) :: Int)
