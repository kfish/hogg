module Main where

import Ogg.Utils
import qualified Data.ByteString.Lazy as L

import Data.Word (Word8)
import Data.Bits

data OggPage =
  OggPage {
    raw :: [Word8],
    len :: Int,
    cont :: Bool,
    bos :: Bool,
    eos :: Bool,
    gp :: Int,
    serialno :: Int
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
ixSeq off len s = reverse (take len (drop off s))

readPage :: [Word8] -> OggPage
readPage d = OggPage d l cont bos eos gp serialno where
  l = length d
  htype = if l > 5 then d !! 5 else 0
  cont = testBit htype 0
  bos = testBit htype 1
  eos = testBit htype 2
  gp = fromTwosComp $ ixSeq 6 8 d
  serialno = fromTwosComp $ ixSeq 14 4 d

instance Show OggPage where

  show (OggPage d l cont bos eos gp serialno) = "serialno " ++ show serialno ++ ", granulepos " ++ show gp ++ flags ++ ": " ++ show l ++ " bytes\n"
    where flags = ifc ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""

main :: IO ()
main = do input <- L.getContents
          putStrLn (show (map readPage (pageSplit $ L.unpack input)))

