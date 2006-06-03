module Main where

import Ogg.Utils
import qualified Data.ByteString.Lazy as L

import Data.Word (Word8)
import Data.Bits

data OggPage =
  OggPage {
    raw :: [Word8],
    len :: Int,
    gp :: Int
  }

pageMarker :: [Word8]
pageMarker = [0x4f, 0x67, 0x67, 0x53] -- "OggS"

pageSplit :: [Word8] -> [[Word8]]
pageSplit = _pageSplit [] []

_pageSplit :: [Word8] -> [[Word8]] -> [Word8] -> [[Word8]]
_pageSplit [] l [] = l
_pageSplit c l [] = l++[c]
_pageSplit c l (r1:r2:r3:r4:r)
    | [r1,r2,r3,r4] == pageMarker = _pageSplit pageMarker (l++[c]) r
    | otherwise                   = _pageSplit (c++[r1]) l (r2:r3:r4:r)
_pageSplit c l r = _pageSplit (c++r) l []

pageCount = length . pageSplit

ixSeq :: Int -> Int -> [Word8] -> [Word8]
ixSeq i j s = take j (drop i s)

readPage :: [Word8] -> OggPage
readPage d = OggPage d (length d) gp where
  gp = fromTwosComp (ixSeq 6 14 d)

instance Show OggPage where

  show (OggPage d l gp) = "Page of length " ++ show l ++ " gp " ++ show gp ++ "\n"

main :: IO ()
main = do input <- L.getContents
          putStrLn (show (map readPage (pageSplit $ L.unpack input)))

