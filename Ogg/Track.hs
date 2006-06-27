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

import Data.Word (Word8, Word32)

------------------------------------------------------------
-- Data
--

data OggType = Vorbis | Speex | Theora
  -- deriving Eq

instance Eq OggType where
  Vorbis == Vorbis = True
  Speex == Speex = True
  Theora == Theora = True
  _ == _ = False

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

vorbisIdent :: [Word8]
vorbisIdent = [0x01, 0x76, 0x6f, 0x72, 0x62, 0x69, 0x73]

theoraIdent :: [Word8]
theoraIdent = [0x80, 0x74, 0x68, 0x65, 0x6f, 0x72, 0x61]

speexIdent :: [Word8]
speexIdent = [0x53, 0x70, 0x65, 0x65, 0x78, 0x20, 0x20, 0x20]

readCType :: [Word8] -> Maybe OggType
readCType (r1:r2:r3:r4:r5:r6:r7:r8:_)
  | [r1,r2,r3,r4,r5,r6,r7] == vorbisIdent = Just Vorbis
  | [r1,r2,r3,r4,r5,r6,r7,r8] == speexIdent = Just Speex
  | [r1,r2,r3,r4,r5,r6,r7] == theoraIdent = Just Theora
  | otherwise = Nothing
readCType _ = Nothing

parseType :: Maybe String -> Maybe OggType
parseType (Just "vorbis") = Just Vorbis
parseType (Just "speex") = Just Speex
parseType (Just "theora") = Just Theora
parseType _ = Nothing

------------------------------------------------------------
-- Show
--

instance Show OggTrack where
  show (OggTrack serialno (Just t)) =
    "serialno " ++ show serialno ++ " " ++ show t

  show (OggTrack serialno Nothing) =
    "serialno " ++ show serialno ++ " (Unknown)"

instance Show OggType where
  show Vorbis = "Vorbis"
  show Speex  = "Speex"
  show Theora = "Theora"
