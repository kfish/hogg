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
import Codec.Container.Ogg.Headers
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Serial

------------------------------------------------------------
-- Exposed functions
--

merge :: [[OggPage]] -> [OggPage]
merge = sortHeaders . mergeSkeleton . listMerge

sort :: [OggPage] -> [OggPage]
sort = sortHeaders . listMerge . demux

------------------------------------------------------------
-- sortHeaders
--

-- | Ensure the header pages of each track are in the correct order
--   relative to each other.
sortHeaders :: [OggPage] -> [OggPage]
sortHeaders = processHeaders sortHeaders'

sortHeaders' :: [OggPage] -> [OggPage]
sortHeaders' = id

------------------------------------------------------------
-- mergeSkeleton
--

-- | When mergeing multiple files together, ensure that the resulting file
--   contains only one Skeleton track
mergeSkeleton :: [OggPage] -> [OggPage]
mergeSkeleton = id
