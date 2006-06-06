module Main where

import Control.Monad

import System.Environment
import System.IO
import System.Console.GetOpt

import qualified Data.ByteString.Lazy as L
import Ogg.Page
import Ogg.Packet

dumpPackets :: String -> IO ()
dumpPackets filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    mapM_ putStrLn (map show (pages2packets (pageScan $ L.unpack input)))

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
      "pagecount" -> countPages filename
      "pagedump" -> dumpPages filename

