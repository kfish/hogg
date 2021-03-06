{-# OPTIONS_GHC -fno-warn-orphans #-}
--
-- Module      : Skeleton
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Skeleton (
  OggFishead (..),
  OggFisbone (..),
  emptyFishead,
  pageToFishead,
  packetToFishead,
  pageToFisbone,
  packetToFisbone,
  fisheadToPage,
  fisheadToPacket,
  fisboneToPage,
  fisboneToPacket,
  trackToFisbone,
  tracksToFisbones
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List as List
import Data.Maybe
import Data.Word (Word32,Word64)
import Data.Ratio

import Codec.Container.Ogg.ByteFields
import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Granulerate
import Codec.Container.Ogg.MessageHeaders
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Serial
import Codec.Container.Ogg.Timestamp
import Codec.Container.Ogg.Track

------------------------------------------------------------
-- Data
--

data OggFishead =
  OggFishead {
    fisheadPrestime :: Timestamp,
    fisheadBasetime :: Timestamp
  }

data OggFisbone =
  OggFisbone {
    fisboneSerialno :: Serial,
    fisboneNHeaders :: Int,
    fisboneGranulerate :: Granulerate,
    fisboneStartgranule :: Word64,
    fisbonePreroll :: Word32,
    fisboneGranuleshift :: Int,
    fisboneMsgHeaders :: MessageHeaders
  }

------------------------------------------------------------
-- OggSkeleton constants
--

-- fisheadIdent = 'fishead\0'
fisheadIdent :: L.ByteString
fisheadIdent = L.pack [0x66, 0x69, 0x73, 0x68, 0x65, 0x61, 0x64, 0x00]

-- fisboneIdent = 'fisbone\0'
fisboneIdent :: L.ByteString
fisboneIdent = L.pack [0x66, 0x69, 0x73, 0x62, 0x6f, 0x6e, 0x65, 0x00]

-- Skeleton major version generated by this module
vMajor :: Int
vMajor = 3

-- Skeleton minor version generated by this module
vMinor :: Int
vMinor = 0 

-- Offset to message header fields generated by this module
fisboneMHOffset :: Int
fisboneMHOffset = 44

-- Padding after granuleshift, before message headers
fisbonePadding :: L.ByteString
fisbonePadding = L.concat $ List.map u8Fill [z, z, z]

-- Helpers
z :: Int
z = 0

emptyFishead :: OggFishead
emptyFishead = OggFishead zeroTimestamp zeroTimestamp

------------------------------------------------------------
-- pageToFishead, pageToFisbone
--

pageToFishead :: OggPage -> Maybe OggFishead
pageToFishead g = case (pagesToPackets [g]) of
  [] -> Nothing
  p:_ -> packetToFishead p

pageToFisbone :: OggPage -> Maybe OggFisbone
pageToFisbone g = case (pagesToPackets [g]) of
  [] -> Nothing
  p:_ -> packetToFisbone p

------------------------------------------------------------
-- packetToFishead, bsToFishead
--

packetToFishead :: OggPacket -> Maybe OggFishead
packetToFishead (OggPacket d t _ bos _ _) = case bos of
  False -> Nothing
  True -> case (contentTypeIs skeleton t) of
    True  -> Just (bsToFishead d)
    False -> Nothing

bsToFishead :: L.ByteString -> OggFishead
bsToFishead d = OggFishead pt bt
  where
    pt = t 12 20
    bt = t 28 36

    -- | Read a timestamp encoded as a (le64,le64) rational, where a
    -- denominator of 0 is interepreted as the result being 0.
    t o1 o2 = case od of
      0 -> Timestamp (0, 1)
      _ -> Timestamp (on, od)
      where
        on = le64At o1 d
        od = le64At o2 d

------------------------------------------------------------
-- packetToFisbone, bsToFisbone
--

packetToFisbone :: OggPacket -> Maybe OggFisbone
packetToFisbone (OggPacket d t _ _ _ _) =
  case (contentTypeIs skeleton t) of
    True  -> Just (bsToFisbone d)
    False -> Nothing

bsToFisbone :: L.ByteString -> OggFisbone
bsToFisbone d = OggFisbone serial nh gr startg pr gshift mh
  where
    serial = le32At 12 d
    nh = le32At 16 d
    gr = fracRate (le64At 20 d) (le64At 28 d)
    startg = le64At 36 d
    pr = le32At 44 d
    gshift = u8At 48 d
    mh = read $ C.unpack (L.drop 52 d)

------------------------------------------------------------
-- fisheadToPage
--

fisheadToPage :: OggTrack -> OggFishead -> OggPage
fisheadToPage t f = head $ packetsToPages [fisheadToPacket t f]

------------------------------------------------------------
-- fisheadToPacket
--

fisheadToPacket :: OggTrack -> OggFishead -> OggPacket
fisheadToPacket t f = up{packetBOS = True}
  where
    up = uncutPacket d t gp
    d = fisheadWrite f
    gp = Granulepos (Just 0)

fisheadWrite :: OggFishead -> L.ByteString
fisheadWrite (OggFishead p b) = newFisheadData
  where
    newFisheadData = L.concat [hData, pData, bData, uData]
    hData = L.concat [fisheadIdent, le16Fill vMajor, le16Fill vMinor]
    pData = timestampFill p
    bData = timestampFill b
    uData = L.concat $ List.map le32Fill $ take 5 $ repeat z

timestampFill :: Timestamp -> L.ByteString
timestampFill (Timestamp (n,d)) = L.concat $ List.map le64Fill [n, d]

------------------------------------------------------------
-- fisboneToPage
--

fisboneToPage :: OggTrack -> OggFisbone -> OggPage
fisboneToPage t f = head $ packetsToPages [fisboneToPacket t f]

------------------------------------------------------------
-- fisboneToPacket
--

fisboneToPacket :: OggTrack -> OggFisbone -> OggPacket
fisboneToPacket t f = uncutPacket d t gp
  where
    d = fisboneWrite f
    gp = Granulepos (Just 0)

fisboneWrite :: OggFisbone -> L.ByteString
fisboneWrite (OggFisbone s n (Granulerate gr) sg pr gs mhdrs) = newFisboneData
  where
    newFisboneData = L.concat [hData, fData, tData]
    hData = L.concat [fisboneIdent, le32Fill fisboneMHOffset]
    fData = L.concat [sD, nD, grD, sgD, prD, gsD]
    tData = L.concat [fisbonePadding, mhdrsD]

    sD = le32Fill s
    nD = le32Fill n
    grD = L.concat $ List.map le64Fill [numerator gr, denominator gr]
    sgD = le64Fill sg
    prD = le32Fill pr
    gsD = u8Fill gs

    mhdrsD = C.pack $ show mhdrs

------------------------------------------------------------
-- trackToFisbone
--

-- | Create a list of OggFisbones from a list of OggTracks, not including
-- | any OggTracks with unknown ContentType or Granulerate
tracksToFisbones :: [OggTrack] -> [OggFisbone]
tracksToFisbones ts = Data.Maybe.mapMaybe trackToFisbone ts

-- | Create an OggFisbone from a given OggTrack
trackToFisbone :: OggTrack -> Maybe OggFisbone
trackToFisbone (OggTrack serialno (Just ctype) nheaders (Just gr) gs mdata) =
  Just (OggFisbone serialno nheaders gr startgranule pr gsi mhdrs)
  where
    pr = fromIntegral $ preroll ctype
    startgranule = 0
    gsi = maybe 0 id gs -- A Granuleshift of None is represented by 0
    -- The first given content-type is the default to use in skeleton
    mhdrs = mhInsert "Content-Type" (head $ mime ctype) mdata

-- If the pattern match failed, ie. any of the Maybe values were Nothing,
-- then we can't produce a valid Fisbone for this
trackToFisbone _ = Nothing

------------------------------------------------------------
-- Custom Instances
--

instance ContentTypeImplied OggPage where
  contentTypeImplies = pageImplies

pageImplies :: [OggTrack] -> ContentType -> OggPage -> Bool
pageImplies tracks t g = case (pagesToPackets [g]) of
  []  -> False
  p:_ -> contentTypeImplies tracks t p

instance ContentTypeImplied OggPacket where
  contentTypeImplies = packetImplies

packetImplies :: [OggTrack] -> ContentType -> OggPacket -> Bool
packetImplies tracks t p = case (contentTypeIs skeleton p) of
    False -> contentTypeIs t p
    True  -> case (packetBOS p, packetEOS p) of
      (True, _) -> True
      (_, True) -> True
      _         -> case (packetToFisbone p) of
                     Nothing -> False
                     Just fb -> fbPacketImplies tracks t fb

fbPacketImplies :: [OggTrack] -> ContentType -> OggFisbone -> Bool
fbPacketImplies tracks t fb = case matchTracks of
    []       -> False
    sTrack:_ -> contentTypeIs t sTrack
  where
    matchTracks = filter (\x -> trackSerialno x == skelSerial) tracks
    skelSerial = fisboneSerialno fb

