module Main where

scancnt :: Int -> [Char] -> [Char] -> [Char] -> Int
scancnt x r [] t = scancnt (x+1) [] r t
scancnt x _ _ [] = x
scancnt x r (s:ss) (t:ts)
    | s == t	= scancnt x (r++[s]) ss ts
    | otherwise	= scancnt x [] (r++[s]++ss) ts

scan = scancnt 0 []

pagescan = scan "OggS"

main :: IO ()
main = do input <- getContents
          putStrLn ((show (pagescan input)) ++ " pages")

