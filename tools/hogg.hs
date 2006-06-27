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
    allPages <- getPages filename
    return $ pages2packets allPages

pageMatch :: Maybe OggType -> [OggPage] -> [OggPage]
pageMatch Nothing gs = gs
pageMatch (Just t) gs = filter (pageIsType t) gs

packetMatch :: Maybe OggType -> [OggPacket] -> [OggPacket]
packetMatch Nothing ps = ps
packetMatch (Just t) ps = filter (packetIsType t) ps


dumpPackets :: [String] -> IO ()
dumpPackets args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    putStrLn $ "Content-Type: " ++ (show ctype)
    let filename = head filenames
    allPackets <- getPackets filename
    let matchPackets = packetMatch ctype allPackets
    mapM_ putStrLn (map show matchPackets)

countPackets :: [String] -> IO ()
countPackets args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    putStrLn $ "Content-Type: " ++ (show ctype)
    let filename = head filenames
    allPackets <- getPackets filename
    let matchPackets = packetMatch ctype allPackets
    putStrLn $ show (length matchPackets) ++ " packets"

rewritePages :: [String] -> IO ()
rewritePages args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPages <- getPages filename
    let matchPages = pageMatch ctype allPages
    mapM_ L.putStr (map L.pack (map pageWrite matchPages))

rewritePackets :: [String] -> IO ()
rewritePackets args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPackets <- getPackets filename
    let matchPackets = packetMatch ctype allPackets
    mapM_ L.putStr (map L.pack (map pageWrite (packetsToPages matchPackets)))

countrwPages :: [String] -> IO ()
countrwPages args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPages <- getPages filename
    let matchPages = pageMatch ctype allPages
    putStrLn $ show $ length (packetsToPages (pages2packets matchPages))

countPages :: [String] -> IO ()
countPages args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPages <- getPages filename
    let matchPages = pageMatch ctype allPages
    putStrLn $ (show $ length matchPages) ++ " pages"

dumpPages :: [String] -> IO ()
dumpPages args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPages <- getPages filename
    let matchPages = pageMatch ctype allPages
    mapM_ putStrLn (map show matchPages)

getFilename :: [String] -> IO String
getFilename args = return $ last args

main :: IO ()
main = do
    (command:args) <- getArgs
    filename <- getFilename args
    case command of
      "dump" -> dumpPackets args
      "packetcount" -> countPackets args
      "pagecount" -> countPages args
      "pagedump" -> dumpPages args
      "rewrite" -> rewritePages args
      "repacket" -> rewritePackets args
      "countrw" -> countrwPages args
