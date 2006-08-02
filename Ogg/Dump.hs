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

import Data.Char (isSpace, chr, ord)
import qualified Data.ByteString.Lazy as L

import Text.Printf

------------------------------------------------------------
-- Dump
--

-- | Generate a hexdump for a block of data

hexDump :: L.ByteString -> String
hexDump = _hexDump 0 ""

_hexDump :: Int -> String -> L.ByteString -> String
_hexDump o s d
  | L.null d = s
  | otherwise = _hexDump (o+16) (s++lineDump) rest
-- _hexDump _ s [] = s
-- _hexDump o s d = _hexDump (o+16) (s++lineDump) rest
    where (line, rest) = L.splitAt 16 d
          lineDump = spaces 4 ++ offset ++ ": " ++ hexLine ++ spaces hexPad ++ ascLine ++ "\n"
          spaces n = take n $ repeat ' '
          offset = printf "%04x" o

          unline = L.unpack line
          hexLine = hexSpace "" hexList False
          hexPad = 1 + 8*5 - length hexLine
          hexList = map hexByte unline
          hexByte x = printf "%02x" ((fromIntegral x)::Int)
          hexSpace x [] _ = x
          hexSpace x (c:cs) True = hexSpace (x++c++" ") cs False
          hexSpace x (c:cs) False = hexSpace (x++c) cs True

          ascLine = concat $ map ascByte chars
          chars = map chr (map fromIntegral unline)
          ascByte c
            | (ord c) > 126 = "."
            | isSpace c = " "
            | (ord c) < 32 = "."
            | otherwise = [c]

