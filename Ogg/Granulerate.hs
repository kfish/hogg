--
-- Module      : Granulerate
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Granulerate (
  Granulerate (..)
) where

import Data.Ratio

------------------------------------------------------------
-- Types
--

newtype Granulerate = Granulerate Rational

------------------------------------------------------------
-- Granulepos functions
--

instance Show Granulerate where
  show (Granulerate r)
    | d == 1    = show n
    | otherwise = show r
    where n = numerator r
          d = denominator r
