module Main where

import Data.Bits

pageSplit :: [CUChar] -> [[CUChar]]
pageSplit = _pageSplit [] []

_pageSplit :: [CUChar] -> [[CUChar]] -> [CUChar] -> [[CUChar]]
_pageSplit [] l [] = l
_pageSplit c l [] = l++[c]
_pageSplit c l ('O':'g':'g':'S':r) = _pageSplit "OggS" (l++[c]) r
_pageSplit c l (r:rs) = _pageSplit (c++[r]) l rs

pageCount = length . pageSplit

main :: IO ()
main = do input <- getContents
          putStrLn ((show (pageCount input)) ++ " pages")

