--
-- Module      : Track
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Track (
  OggTrack (..),
  OggType (..),
  trackIsType,
  newTrack,
  nullTrack,
  bosToTrack,
  nheadersOf,
  prerollOf,
  parseType,
  gpToTimestamp
) where

import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Word (Word32)
import Data.Ratio

import Text.Printf

import Ogg.ByteFields
import Ogg.Granulepos
import Ogg.Granulerate
import Ogg.Timestamp

------------------------------------------------------------
-- Data
--

data OggType = Skeleton | CMML | Vorbis | Speex | Theora
  deriving Eq

data OggTrack =
  OggTrack {
    trackSerialno :: Word32,
    trackType :: Maybe OggType,
    trackGranulerate :: Maybe Granulerate,
    trackGranuleshift :: Maybe Int
  }

------------------------------------------------------------
--
--

-- | Predicate
trackIsType :: OggType -> OggTrack -> Bool
trackIsType t0 track
  | (Just t0) == t1  = True
  | otherwise        = False
  where t1 = trackType track

-- | The null track
nullTrack :: OggTrack
nullTrack = OggTrack 0 Nothing Nothing Nothing

-- | A new track, with a DEFAULT serialno
-- | XXX: This should be a random serialno instead
newTrack :: OggTrack
newTrack = OggTrack 777 Nothing Nothing Nothing

-- Instantiate an OggTrack given a serialno and a bos page
bosToTrack :: Word32 -> L.ByteString -> OggTrack
bosToTrack s d = OggTrack s ctype gr gs
  where
    ctype = readCType d
    gr = readGR ctype d
    gs = readGS ctype d

-- | Convert a granulepos to a timestamp
gpToTimestamp :: Granulepos -> OggTrack -> Timestamp
gpToTimestamp mgp track
  | g == Nothing = Timestamp Nothing
  | r == Nothing = Timestamp Nothing
  | otherwise    = Timestamp timestamp
  where g = gpToGranules mgp track
        r = trackGranulerate track
        timestamp = Just (fromIntegral granules*d, fromIntegral n)
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

-- skeletonIdent = 'fishead\0'
skeletonIdent :: L.ByteString
skeletonIdent = L.pack [0x66, 0x69, 0x73, 0x68, 0x65, 0x61, 0x64, 0x00]

-- cmmlIdent = 'CMML\0\0\0\0\'
cmmlIdent :: L.ByteString
cmmlIdent = L.pack [0x43, 0x4d, 0x4d, 0x4c, 0x00, 0x00, 0x00, 0x00]

-- vorbisIdent = '\x01vorbis'
vorbisIdent :: L.ByteString
vorbisIdent = L.pack [0x01, 0x76, 0x6f, 0x72, 0x62, 0x69, 0x73]

-- theoraIdent = '\x80theora'
theoraIdent :: L.ByteString
theoraIdent = L.pack [0x80, 0x74, 0x68, 0x65, 0x6f, 0x72, 0x61]

-- speexIdent = 'Speex   '
speexIdent :: L.ByteString
speexIdent = L.pack [0x53, 0x70, 0x65, 0x65, 0x78, 0x20, 0x20, 0x20]

-- | Determine the content type of a bos page
readCType :: L.ByteString -> Maybe OggType
readCType d
  | L.isPrefixOf skeletonIdent d = Just Skeleton
  | L.isPrefixOf cmmlIdent d = Just CMML
  | L.isPrefixOf vorbisIdent d = Just Vorbis
  | L.isPrefixOf speexIdent d = Just Speex
  | L.isPrefixOf theoraIdent d = Just Theora
  | otherwise = Nothing

-- | Read the granulerate from the data of a bos page
readGR :: Maybe OggType -> L.ByteString -> Maybe Granulerate
readGR Nothing _ = Nothing
readGR (Just Skeleton) _ = Nothing
readGR (Just CMML) d = Just (fracRate (le64At 12 d) (le64At 20 d))
readGR (Just Vorbis) d = Just (intRate (le32At 12 d))
readGR (Just Speex) d = Just (intRate (le32At 36 d))
readGR (Just Theora) d = Just (fracRate (be32At 22 d) (be32At 26 d))

-- | Read the granuleshift from the data of a bos page
readGS :: Maybe OggType -> L.ByteString -> Maybe Int
readGS Nothing _ = Nothing
readGS (Just CMML) d = Just (u8At 28 d)
readGS (Just Theora) d = Just (h40 .|. h41)
  where h40 = (u8At 40 d .&. 0x03) `shiftL` 3
        h41 = (u8At 41 d .&. 0xe0) `shiftR` 5
readGS _ _ = Nothing

-- | Parse the specification of a content-type
parseType :: Maybe String -> Maybe OggType
parseType (Just "skeleton") = Just Skeleton
parseType (Just "cmml") = Just CMML
parseType (Just "vorbis") = Just Vorbis
parseType (Just "speex") = Just Speex
parseType (Just "theora") = Just Theora
parseType _ = Nothing

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
-- Content-Type specific behaviours
--

nheadersOf :: OggType -> Int
nheadersOf CMML = 3
nheadersOf Speex = 3
nheadersOf Theora = 3
nheadersOf Vorbis = 3
nheadersOf _ = 1

prerollOf :: OggType -> Int
prerollOf Vorbis = 2
prerollOf Speex = 3
prerollOf _ = 0


------------------------------------------------------------
-- Show
--

instance Show OggTrack where
  -- show (OggTrack serialno (Just t) (Just gr)) =
  show (OggTrack serialno ctype gr gs) =
    t ++ ": serialno " ++ s ++ " Rate: " ++ g ++ " Shift: " ++ sgs ++ "\n"
    where s = printf "%010d" ((fromIntegral serialno) :: Int)
          t = maybe "(Unknown)" show ctype
          g = maybe "--" show gr
          sgs = maybe "None" show gs

  -- show (OggTrack serialno _ _) =
  --   "(Unknown): serialno " ++ s ++ "\n"
  --   where s = printf "%010d" ((fromIntegral serialno) :: Int)

instance Show OggType where
  show Skeleton = "Skeleton"
  show CMML = "CMML"
  show Vorbis = "Vorbis"
  show Speex  = "Speex"
  show Theora = "Theora"
