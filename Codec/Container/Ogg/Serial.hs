{-# LANGUAGE TypeSynonymInstances #-}
-- Module      : Serial
-- Copyright   : (c) Conrad Parker 2007
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Serial (
  Serial,
  Serialled,
  serialOf,
  demux,
  genSerial
) where

import Data.Word (Word32)
import System.Random
import Codec.Container.Ogg.List

type Serial = Word32

------------------------------------------------------------
-- | Typeclass: Serialled
--

class Serialled a where
  serialOf :: a -> Serial

serialEq :: (Serialled a, Serialled b) => a -> b -> Bool
serialEq a b = (serialOf a) == (serialOf b)

------------------------------------------------------------
-- | Demux
--

demux :: (Serialled a) => [a] -> [[a]]
demux = classify serialEq

------------------------------------------------------------
-- | Generate a serial number
--

-- Make a special instance of Random for Serial that does not include
-- 0xffffffff, as this value is treated specailly by libogg
instance Random Serial where
  randomR = integralRandomR
  random = randomR (0,0xffffffff-1)

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g') -> (fromIntegral x, g')

genSerial :: IO Serial
genSerial = do
  serialno <- getStdRandom random
  return serialno
