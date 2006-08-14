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
  readCType,
  parseType
) where

import Data.Word (Word32)
import qualified Data.ByteString.Lazy as L

------------------------------------------------------------
-- Data
--

data OggType = Skeleton | CMML | Vorbis | Speex | Theora
  deriving Eq

data OggTrack =
  OggTrack {
    trackSerialno :: Word32,
    trackType :: Maybe OggType
  }

------------------------------------------------------------
--
--

trackIsType :: OggType -> OggTrack -> Bool
trackIsType t0 (OggTrack _ (Just t1))
  | t0 == t1  = True
  | otherwise = False
trackIsType _ _ = False

nullTrack :: OggTrack
nullTrack = OggTrack 0 Nothing

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

parseType :: Maybe String -> Maybe OggType
parseType (Just "skeleton") = Just Skeleton
parseType (Just "cmml") = Just CMML
parseType (Just "vorbis") = Just Vorbis
parseType (Just "speex") = Just Speex
parseType (Just "theora") = Just Theora
parseType _ = Nothing

-- | Tracks are equal if their serialnos are equal
instance Eq OggTrack where
  (==) (OggTrack s1 _) (OggTrack s2 _) = s1 == s2

instance Ord OggTrack where
  compare (OggTrack s1 _) (OggTrack s2 _) = compare s1 s2

------------------------------------------------------------
-- Show
--

instance Show OggTrack where
  show (OggTrack serialno (Just t)) =
    "serialno " ++ show serialno ++ " " ++ show t

  show (OggTrack serialno Nothing) =
    "serialno " ++ show serialno ++ " (Unknown)"

instance Show OggType where
  show Skeleton = "Skeleton"
  show CMML = "CMML"
  show Vorbis = "Vorbis"
  show Speex  = "Speex"
  show Theora = "Theora"
