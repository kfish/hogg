--
-- Module      : Serial
-- Copyright   : (c) Conrad Parker 2007
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Serial (
  Serial,
  genSerial
) where

import Data.Word (Word32)
import System.Random

type Serial = Word32

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
