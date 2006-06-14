--
-- Module      : Dump
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Dump (
  hexDump
) where

import Data.Word (Word8)
import Data.Char (isSpace, chr, ord)

import Text.Printf

------------------------------------------------------------
-- Dump
--

-- | Generate a hexdump for a block of data

hexDump :: [Word8] -> String
hexDump = _hexDump 0 ""

_hexDump :: Int -> String -> [Word8] -> String
_hexDump _ s [] = s
_hexDump o s d = _hexDump (o+16) (s++lineDump) rest
    where (line, rest) = splitAt 16 d
          lineDump = spaces 4 ++ offset ++ ": " ++ hexLine ++ spaces hexPad ++ ascLine ++ "\n"
          spaces n = take n $ repeat ' '
          offset = printf "%04x" o

          hexLine = hexSpace "" hexList False
          hexPad = 1 + 8*5 - length hexLine
          hexList = map hexByte line
          hexByte x = printf "%02x" ((fromIntegral x)::Int)
          hexSpace x [] _ = x
          hexSpace x (c:cs) True = hexSpace (x++c++" ") cs False
          hexSpace x (c:cs) False = hexSpace (x++c) cs True

          ascLine = concat $ map ascByte chars
          chars = map chr (map fromIntegral line)
          ascByte c
            | (ord c) > 126 = "."
            | isSpace c = " "
            | (ord c) < 32 = "."
            | otherwise = [c]

