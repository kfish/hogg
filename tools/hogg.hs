module Main where

import System.Environment
import System.Console.GetOpt

import qualified Data.ByteString.Lazy as L
import Ogg.Page
import Ogg.Packet

dumpPackets :: IO ()
dumpPackets = do
    input <- L.getContents
    mapM_ putStrLn (map show (pages2packets (pageScan $ L.unpack input)))

countPages :: IO ()
countPages = do
    input <- L.getContents
    putStrLn $ (show $ length (pageScan $ L.unpack input)) ++ " pages"

dumpPages :: IO ()
dumpPages = do
    input <- L.getContents
    mapM_ putStrLn (map show (pageScan $ L.unpack input))

main :: IO ()
main = do
    (command:args) <- getArgs
    case command of
      "dump" -> dumpPackets
      "pagecount" -> countPages
      "pagedump" -> dumpPages

