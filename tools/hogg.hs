module Main where

import Control.Monad

import System.Environment
import System.IO
import System.Console.GetOpt

import qualified Data.ByteString.Lazy as L
import Ogg.Page
import Ogg.Packet

countPackets :: String -> IO ()
countPackets filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    putStrLn $ show (length (pages2packets (pageScan $ L.unpack input))) ++ " packets"

dumpPackets :: String -> IO ()
dumpPackets filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    mapM_ putStrLn (map show (pages2packets (pageScan $ L.unpack input)))

rewritePages :: String -> IO ()
rewritePages filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    mapM_ putStrLn (map show (pages2packets (pageScan $ L.unpack input)))
    -- mapM_ L.putStr (map L.pack (map pageWrite (pageScan $ L.unpack input)))

testPages :: String -> IO ()
testPages filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    mapM_ putStrLn (map pageTest (pageScan $ L.unpack input))

countPages :: String -> IO ()
countPages filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    putStrLn $ (show $ length (pageScan $ L.unpack input)) ++ " pages"

dumpPages :: String -> IO ()
dumpPages filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    mapM_ putStrLn (map show (pageScan $ L.unpack input))

getFilename :: [String] -> IO String
getFilename args = return $ last args

main :: IO ()
main = do
    (command:args) <- getArgs
    filename <- getFilename args
    case command of
      "dump" -> dumpPackets filename
      "packetcount" -> countPackets filename
      "pagecount" -> countPages filename
      "pagedump" -> dumpPages filename
      "rewrite" -> rewritePages filename
      "test" -> testPages filename
