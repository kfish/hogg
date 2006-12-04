--
-- Module      : ContentType
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.ContentType (
  ContentType (..),
  identify,
  granulerate,
  granuleshift,
  parseType,

  -- Some guaranteed-known content-types
  skeleton
) where

import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Map (fromList)
import Data.Maybe

import Text.Printf

import Codec.Container.Ogg.ByteFields
import Codec.Container.Ogg.Granulerate
import Codec.Container.Ogg.MessageHeaders

------------------------------------------------------------
-- Data
--

data ContentType =
  ContentType {
    label :: String,
    mime :: [String],
    identifyP :: L.ByteString -> Bool, -- predictate, used by identify
    headers :: Int,
    preroll :: Int,
    granulerateF :: Maybe (L.ByteString -> Granulerate), -- used by granulerate
    granuleshiftF :: Maybe (L.ByteString -> Int), -- used by granuleshift
    metadata :: L.ByteString -> MessageHeaders
  }

known :: [ContentType]
known = [skeleton, cmml, vorbis, theora, speex]

identify :: L.ByteString -> Maybe ContentType
identify d = listToMaybe $ filter (\x -> identifyP x d) known

granulerate :: ContentType -> L.ByteString -> Maybe Granulerate
granulerate c d = maybe Nothing (\f -> Just (f d)) (granulerateF c)

granuleshift :: ContentType -> L.ByteString -> Maybe Int
granuleshift c d = maybe Nothing (\f -> Just (f d)) (granuleshiftF c)

parseType :: Maybe String -> Maybe ContentType
parseType Nothing = Nothing
parseType (Just s) = listToMaybe $ filter (\x ->  l (label x) == l s) known
  where
    l = map toLower

instance Eq ContentType where
  (==) a b = label a == label b

instance Show ContentType where
  show x = label x

------------------------------------------------------------
-- Skeleton
--

skeleton :: ContentType
skeleton = ContentType
             "Skeleton"                     -- label
             ["application/x-ogg-skeleton"] -- mime
             (L.isPrefixOf skeletonIdent)   -- identify
             0                              -- headers
             0                              -- preroll
             Nothing                        -- granulerate
             Nothing                        -- granuleshift
             (const mhEmpty)

-- skeletonIdent = 'fishead\0'
skeletonIdent :: L.ByteString
skeletonIdent = L.pack [0x66, 0x69, 0x73, 0x68, 0x65, 0x61, 0x64, 0x00]

------------------------------------------------------------
-- CMML
--

cmml :: ContentType
cmml = ContentType
         "CMML"                   -- label
         ["text/x-cmml"]          -- mime
         (L.isPrefixOf cmmlIdent) -- identify
         3                        -- headers
         0                        -- preroll
         (Just (\d -> fracRate (le64At 12 d) (le64At 20 d))) -- granulerate
         (Just (\d -> u8At 28 d)) -- granuleshift
             (const mhEmpty)

-- cmmlIdent = 'CMML\0\0\0\0\'
cmmlIdent :: L.ByteString
cmmlIdent = L.pack [0x43, 0x4d, 0x4d, 0x4c, 0x00, 0x00, 0x00, 0x00]

------------------------------------------------------------
-- Vorbis
--

vorbis :: ContentType
vorbis = ContentType
           "Vorbis"                   -- label
           ["audio/x-vorbis"]         -- mime
           (L.isPrefixOf vorbisIdent) -- identify
           3                          -- headers
           2                          -- preroll
           (Just (\d -> intRate (le32At 12 d))) -- granulerate
           Nothing                    -- granuleshift
             (const mhEmpty)

-- vorbisIdent = '\x01vorbis'
vorbisIdent :: L.ByteString
vorbisIdent = L.pack [0x01, 0x76, 0x6f, 0x72, 0x62, 0x69, 0x73]

------------------------------------------------------------
-- Theora
--

theora :: ContentType
theora = ContentType
           "Theora"                   -- label
           ["video/x-theora"]         -- mime
           (L.isPrefixOf theoraIdent) -- identify
           3                          -- headers
           0                          -- preroll
           (Just (\d -> fracRate (be32At 22 d) (be32At 26 d))) -- granulerate
           (Just theoraGranuleshift)  -- granuleshift
           theoraMetadata             -- metadata

-- theoraIdent = '\x80theora'
theoraIdent :: L.ByteString
theoraIdent = L.pack [0x80, 0x74, 0x68, 0x65, 0x6f, 0x72, 0x61]

-- Theora's granuleshift is an 8 bit field split over two bytes
theoraGranuleshift :: L.ByteString -> Int
theoraGranuleshift d = (h40 .|. h41)
  where h40 = (u8At 40 d .&. 0x03) `shiftL` 3
        h41 = (u8At 41 d .&. 0xe0) `shiftR` 5

theoraMetadata :: L.ByteString -> MessageHeaders
theoraMetadata d = MessageHeaders (fromList headerVals)
  where headerVals = [framerate, width, height]
        framerate = ("Video-Framerate", [printf "%.3f fps" fps])
        width = ("Video-Width", [show w])
        height = ("Video-Height", [show h])
        fps :: Double
        fps = fromIntegral (be32At 22 d) / fromIntegral (be32At 26 d)
        w = ((be16At 10 d) * 16) :: Int
        h = ((be16At 12 d) * 16) :: Int

------------------------------------------------------------
-- Speex
--

speex :: ContentType
speex = ContentType
          "Speex"                   -- label
          ["audio/x-speex"]         -- mime
          (L.isPrefixOf speexIdent) -- identify
          3                         -- headers
          3                         -- preroll
          (Just (\d -> intRate (le32At 36 d))) -- granulerate
          Nothing                   -- granuleshift
             (const mhEmpty)
          
-- speexIdent = 'Speex   '
speexIdent :: L.ByteString
speexIdent = L.pack [0x53, 0x70, 0x65, 0x65, 0x78, 0x20, 0x20, 0x20]

