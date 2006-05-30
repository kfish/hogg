module Main where

_scan :: [Char] -> [Char] -> [Char] -> Bool
_scan _ [] _ = True
_scan _ _ [] = False
_scan r (s:ss) (t:ts)
    | s == t	= _scan (r++[s]) ss ts
    | otherwise	= _scan [] (r++[s]++ss) ts

scan = _scan []

pagescan = scan "OggS"

main :: IO ()
main = do input <- getContents
          putStrLn (show (pagescan input))

