module Main where

import Control.Monad

import System.Environment (getArgs, getProgName)
import System.IO

import System.Console.GetOpt
import System.Exit

import qualified Data.ByteString.Lazy as L
import Ogg.Page
import Ogg.Packet
import Ogg.Track

countPackets :: String -> IO ()
countPackets filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    putStrLn $ show (length (pages2packets (pageScan $ L.unpack input))) ++ " packets"

------------------------------------------------------------
-- Options processing
--

data Config =
  Config {
    contentTypeCfg :: Maybe String,
    files :: [FilePath]
  }

dftConfig =
  Config {
    contentTypeCfg = Nothing,
    files = ["-"]
  }

-- Available options
--
data Option = Help
            | ContentTypeOpt String
            deriving Eq

options :: [OptDescr Option]
options = [ Option ['h', '?'] ["help"] (NoArg Help)
              "Display this help and exit"
          , Option ['c']      ["content-type"] (ReqArg ContentTypeOpt "Content-Type")
              "Dump only the logical bitstreams for a specified content type."
          ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
  case getOpt RequireOrder options args of
    (opts, args  , []  ) -> do
                        processHelp opts
                        config <- processConfig dftConfig opts
                        return (config, args)
    -- (opts, args, []  ) -> abort [unrecErr ++ unwords args]
    -- (_   , _   , errs) -> abort errs
  where
    unrecErr = "Unrecognised arguments: "

processHelp :: [Option] -> IO ()
processHelp opts = do
  name <- getProgName
  let header = "\nUsage: " ++ name ++ "[options] filename\n"
  when (Help `elem` opts) $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess
  return ()

processConfig :: Config -> [Option] -> IO Config
processConfig = foldM processOneOption
  where
    processOneOption config (ContentTypeOpt ctype) =
      return $ config {contentTypeCfg = Just ctype}

getPages :: FilePath -> IO [OggPage]
getPages filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    return $ pageScan (L.unpack input)

getPackets :: FilePath -> IO [OggPacket]
getPackets filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    return $ pages2packets (pageScan $ L.unpack input)

pageMatch :: Maybe OggType -> [OggPage] -> [OggPage]
pageMatch Nothing gs = gs
pageMatch (Just t) gs = filter (pageIsType t) gs

packetMatch :: Maybe OggType -> [OggPacket] -> [OggPacket]
packetMatch Nothing ps = ps
packetMatch (Just t) ps = filter (packetIsType t) ps


dumpPackets :: [String] -> IO ()
dumpPackets args = do
    -- let filename = last args
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    putStrLn $ "Content-Type: " ++ (show ctype)
    let filename = head filenames
    allPackets <- getPackets filename
    let matchPackets = packetMatch ctype allPackets
    mapM_ putStrLn (map show matchPackets)

rewritePages :: [String] -> IO ()
rewritePages args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPages <- getPages filename
    let matchPages = pageMatch ctype allPages
    mapM_ L.putStr (map L.pack (map pageWrite matchPages))

    -- handle <- openFile filename ReadMode
    -- input <- L.hGetContents handle
    -- mapM_ L.putStr (map L.pack (map pageWrite (pageScan $ L.unpack input)))

rewritePackets :: [String] -> IO ()
rewritePackets args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPackets <- getPackets filename
    let matchPackets = packetMatch ctype allPackets
    mapM_ L.putStr (map L.pack (map pageWrite (packetsToPages matchPackets)))

countrwPages :: String -> IO ()
countrwPages filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    putStrLn $ show $ length (packetsToPages (pages2packets (pageScan $ L.unpack input)))

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
      "dump" -> dumpPackets args
      "packetcount" -> countPackets filename
      "pagecount" -> countPages filename
      "pagedump" -> dumpPages filename
      "rewrite" -> rewritePages args
      "repacket" -> rewritePackets args
      "countrw" -> countrwPages filename
