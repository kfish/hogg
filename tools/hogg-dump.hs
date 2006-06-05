module Main where

import qualified Data.ByteString.Lazy as L
import Ogg.Demux
import Ogg.Packet

main :: IO ()
main = do input <- L.getContents
          mapM_ putStrLn (map show (pages2packets (map readPage (pageSplit $ L.unpack input))))

