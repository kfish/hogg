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
  nullTrack,
  bosToTrack,
  parseType
) where

import qualified Data.ByteString.Lazy as L
import Data.Word (Word32)
import Data.Ratio

import Text.Printf

import Ogg.ByteFields
import Ogg.Granulerate

------------------------------------------------------------
-- Data
--

data OggType = Skeleton | CMML | Vorbis | Speex | Theora
  deriving Eq

data OggTrack =
  OggTrack {
    trackSerialno :: Word32,
    trackType :: Maybe OggType,
    trackGranulerate :: Maybe Granulerate
  }

------------------------------------------------------------
--
--

trackIsType :: OggType -> OggTrack -> Bool
trackIsType t0 track
  | (Just t0) == t1  = True
  | otherwise        = False
  where t1 = trackType track

nullTrack :: OggTrack
nullTrack = OggTrack 0 Nothing Nothing

-- bosToTrack
bosToTrack :: Word32 -> L.ByteString -> OggTrack
bosToTrack s d = OggTrack s ctype gr
  where
    ctype = readCType d
    gr = readGR ctype d

-- skeletonIdent = 'fishead\0'
skeletonIdent :: L.ByteString
skeletonIdent = L.pack [0x66, 0x69, 0x73, 0x68, 0x65, 0x61, 0x64, 0x00]

-- cmmlIdent = 'CMML\0\0\0\0\'
cmmlIdent :: L.ByteString
cmmlIdent = L.pack [0x43, 0x4d, 0x4d, 0x4c, 0x00, 0x00, 0x00, 0x00]

vorbisIdent :: L.ByteString
vorbisIdent = L.pack [0x01, 0x76, 0x6f, 0x72, 0x62, 0x69, 0x73]

theoraIdent :: L.ByteString
theoraIdent = L.pack [0x80, 0x74, 0x68, 0x65, 0x6f, 0x72, 0x61]

speexIdent :: L.ByteString
speexIdent = L.pack [0x53, 0x70, 0x65, 0x65, 0x78, 0x20, 0x20, 0x20]

readCType :: L.ByteString -> Maybe OggType
readCType d
  | L.isPrefixOf skeletonIdent d = Just Skeleton
  | L.isPrefixOf cmmlIdent d = Just CMML
  | L.isPrefixOf vorbisIdent d = Just Vorbis
  | L.isPrefixOf speexIdent d = Just Speex
  | L.isPrefixOf theoraIdent d = Just Theora
  | otherwise = Nothing

-- readCType (r1:r2:r3:r4:r5:r6:r7:r8:_)
--   | [r1,r2,r3,r4,r5,r6,r7] == vorbisIdent = Just Vorbis
--   | [r1,r2,r3,r4,r5,r6,r7,r8] == speexIdent = Just Speex
--   | [r1,r2,r3,r4,r5,r6,r7] == theoraIdent = Just Theora
--   | otherwise = Nothing
-- readCType _ = Nothing

readGR :: Maybe OggType -> L.ByteString -> Maybe Granulerate
readGR Nothing _ = Nothing
readGR (Just Skeleton) _ = Nothing
-- readGR (Just Speex) d = Just (Granulerate (le32At 36 d % 1))
readGR (Just Speex) d = Just (intRate (le32At 36 d))
readGR _ _ = Nothing

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
-- Show
--

instance Show OggTrack where
  -- show (OggTrack serialno (Just t) (Just gr)) =
  show (OggTrack serialno ctype gr) =
    t ++ ": serialno " ++ s ++ " Rate: " ++ g ++ "\n"
    where s = printf "%010d" ((fromIntegral serialno) :: Int)
          t = maybe "(Unknown)" show ctype
          g = maybe "--" show gr

  -- show (OggTrack serialno _ _) =
  --   "(Unknown): serialno " ++ s ++ "\n"
  --   where s = printf "%010d" ((fromIntegral serialno) :: Int)

instance Show OggType where
  show Skeleton = "Skeleton"
  show CMML = "CMML"
  show Vorbis = "Vorbis"
  show Speex  = "Speex"
  show Theora = "Theora"
