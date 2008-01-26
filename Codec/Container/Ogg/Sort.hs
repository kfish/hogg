--
-- Module      : Sort
-- Copyright   : (c) Conrad Parker 2008
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Sort (
  merge,
  sort,
  --, sortHeaders
) where

import Codec.Container.Ogg.List
import Codec.Container.Ogg.Serial

------------------------------------------------------------
-- Functions
--

merge :: (Ord a) => [[a]] -> [a]
merge = sortHeaders . listMerge

sort :: (Ord a, Serialled a) => [a] -> [a]
sort = merge . demux

sortHeaders :: [a] -> [a]
sortHeaders = id
