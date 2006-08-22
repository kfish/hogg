--
-- Module      : ListMerge
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.ListMerge (
  listMerge
) where

import Data.List

listMerge :: Ord a => [[a]] -> [a]
listMerge ll = listMerge' $ listSort ll

listMerge' :: Ord a => [[a]] -> [a]
listMerge' [] = []
listMerge' (l:ls) = case l of
  [] -> listMerge' ls
  (x:xs) -> x : listMerge (xs:ls)

listSort :: Ord a => [[a]] -> [[a]]
listSort xs = sortBy listOrd xs

listOrd :: Ord a => [a] -> [a] -> Ordering
listOrd [] _ = LT
listOrd _ [] = GT
listOrd (x:xs) (y:ys) = compare x y

