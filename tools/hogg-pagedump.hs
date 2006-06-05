module Main where

import qualified Data.ByteString.Lazy as L
import Ogg.Page

main :: IO ()
main = do input <- L.getContents
          mapM_ putStrLn (map (show . readPage) (pageSplit $ L.unpack input))

