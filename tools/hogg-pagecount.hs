module Main where

import qualified Data.ByteString.Lazy as L
import Ogg.Page

main :: IO ()
main = do input <- L.getContents
          putStrLn $ (show $ length (pageScan $ L.unpack input)) ++ " pages"

