module Main where

import Control.Monad
import Control.Exception

import System.Environment (getArgs, getProgName)
import System.IO

import System.Console.GetOpt
import System.Exit

import Text.Printf

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Ogg.Chain
import Ogg.RawPage
import Ogg.Page
import Ogg.Packet
import Ogg.Track
import Ogg.ListMerge

------------------------------------------------------------
-- Options processing
--

data Config =
  Config {
    contentTypeCfg :: Maybe String,
    outputCfg :: Maybe String,
    files :: [FilePath]
  }

dftConfig =
  Config {
    contentTypeCfg = Nothing,
    outputCfg = Nothing,
    files = ["-"]
  }

-- Options available for subcommands
--
data Option = Help
            | ContentTypeOpt String
            | OutputOpt String
            deriving Eq

options :: [OptDescr Option]
options = [ Option ['h', '?'] ["help"] (NoArg Help)
              "Display this help and exit"
          , Option ['c']      ["content-type"] (ReqArg ContentTypeOpt "Content-Type")
              "Select the logical bitstreams for a specified content type"
          , Option ['o']      ["output"] (ReqArg OutputOpt "filename")
              "Specify output filename"
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
  let header = "\nUsage: " ++ name ++ " [options] filename\n"
  when (Help `elem` opts) $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess
  return ()

processConfig :: Config -> [Option] -> IO Config
processConfig = foldM processOneOption
  where
    processOneOption config (ContentTypeOpt ctype) =
      return $ config {contentTypeCfg = Just ctype}
    processOneOption config (OutputOpt output) =
      return $ config {outputCfg = Just output}

getChain :: FilePath -> IO [OggChain]
getChain filename = do
  handle <- openFile filename ReadMode
  input <- L.hGetContents handle
  return $ chainScan input

getTracks :: FilePath -> IO [OggTrack]
getTracks filename = do
    chain <- getChain filename
    return $ chainTracks $ head chain

getRawPages :: FilePath -> IO [OggRawPage]
getRawPages filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    return $ rawPageScan input

getPages :: FilePath -> IO [OggPage]
getPages filename = do
    chain <- getChain filename
    return $ chainPages $ head chain

getPackets :: FilePath -> IO [OggPacket]
getPackets filename = do
    chain <- {-# SCC "getChain" #-}getChain filename
    return $ chainPackets $ head chain

trackMatch :: Maybe OggType -> [OggTrack] -> [OggTrack]
trackMatch Nothing ts = ts
trackMatch (Just t) ts = filter (trackIsType t) ts

pageMatch :: Maybe OggType -> [OggPage] -> [OggPage]
pageMatch Nothing gs = gs
pageMatch (Just t) gs = filter (pageIsType t) gs

packetMatch :: Maybe OggType -> [OggPacket] -> [OggPacket]
packetMatch Nothing ps = ps
packetMatch (Just t) ps = filter (packetIsType t) ps

mTracks :: Config -> [String] -> IO [OggTrack]
mTracks config filenames = do
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allTracks <- getTracks filename
    return $ trackMatch ctype allTracks

mRawPages :: Config -> [String] -> IO [OggRawPage]
mRawPages config filenames = do
    -- let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allRawPages <- getRawPages filename
    return allRawPages

mPages :: Config -> String -> IO [OggPage]
mPages config filename = do
    let ctype = parseType $ contentTypeCfg config
    allPages <- getPages filename
    return $ pageMatch ctype allPages

mPackets :: Config -> [String] -> IO [OggPacket]
mPackets config filenames = do
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPackets <- {-# SCC "getPackets" #-}getPackets filename
    return $ packetMatch ctype allPackets

outputHandle :: Config -> IO Handle
outputHandle config =
    maybe (evaluate stdout) (\f -> openBinaryFile f WriteMode) (outputCfg config)

outputS :: Config -> String -> IO ()
outputS config s = do
    h <- outputHandle config
    hPutStr h s
    hClose h

outputC :: Config -> C.ByteString -> IO ()
outputC config bs = do
    h <- outputHandle config
    C.hPut h bs
    hClose h

outputL :: Config -> L.ByteString -> IO ()
outputL config bs = do
    h <- outputHandle config
    L.hPut h bs
    hClose h

info :: [String] -> IO ()
info args = do
    (config, filenames) <- processArgs args
    matchTracks <- mTracks config filenames
    outputC config $ C.concat $ map (C.pack . show) matchTracks

dumpPackets :: [String] -> IO ()
dumpPackets args = do
    (config, filenames) <- processArgs args
    matchPackets <- {-# SCC "matchPackets" #-}mPackets config filenames
    outputC config $ C.concat $ map packetToBS matchPackets

countPackets :: [String] -> IO ()
countPackets args = do
    (config, filenames) <- processArgs args
    matchPackets <- mPackets config filenames
    outputS config $ show (length matchPackets) ++ " packets"

rewritePages :: [String] -> IO ()
rewritePages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mPages config filename
    outputL config $ L.concat (map pageWrite matchPages)

rewritePackets :: [String] -> IO ()
rewritePackets args = do
    (config, filenames) <- processArgs args
    matchPackets <- mPackets config filenames
    outputL config $ L.concat (map pageWrite (packetsToPages matchPackets))

countrwPages :: [String] -> IO ()
countrwPages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mPages config filename
    outputS config $ show $ length (packetsToPages (pagesToPackets matchPages))

countPages :: [String] -> IO ()
countPages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mPages config filename
    outputS config $ (show $ length matchPages) ++ " pages"

dumpPages :: [String] -> IO ()
dumpPages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mPages config filename
    outputC config $ C.concat $ map (C.pack . show) matchPages

mergePages :: [String] -> IO ()
mergePages args = do
    (config, filenames) <- processArgs args
    matchPages <- mapM (mPages config) filenames
    outputL config $ L.concat $ map pageWrite $ listMerge matchPages

dumpRawPages :: [String] -> IO ()
dumpRawPages args = do
    (config, filenames) <- processArgs args
    matchPages <- mRawPages config filenames
    outputC config $ C.concat $ map (C.pack . show) matchPages

getFilename :: [String] -> IO String
getFilename args = return $ last args

helpCommand :: String -> String -> IO ()
helpCommand command desc = do
    putStrLn $ printf "  %-14s%s" command desc

helpCommands :: IO ()
helpCommands = do
    putStrLn "Usage: hogg <subcommand> [options] filename ...\n"
    putStrLn "Available subcommands:"
    helpCommand "info" "Display information about the file and its bitstreams"
    helpCommand "dump" "Hexdump packets of an Ogg file"
    helpCommand "pagedump" "Display page structure of an Ogg file"
    helpCommand "dumpraw" "Dump raw (unparsed) page data"
    helpCommand "pagecount" "Count pages of an Ogg file" 
    helpCommand "rip" "Rip selected logical bistreams from an Ogg file (default: all)"
    helpCommand "merge" "Merge, interleaving pages in order of presentation time"
    helpCommand "reconstruct" "Reconstruct an Ogg file by doing a full packet demux"
    helpCommand "countrw" "Rewrite an Ogg file via packets and display a count"
    -- helpCommand "help" "Display this help and exit"
    putStrLn "\nPlease report bugs to <ogg-dev@xiph.org>"

main :: IO ()
main = do
    allArgs <- getArgs
    case allArgs of
      []             -> helpCommands
      (command:args) -> do
        case command of
          "help" -> helpCommands
          "info" -> info args
          "dump" -> dumpPackets args
          "packetcount" -> countPackets args
          "pagecount" -> countPages args
          "pagedump" -> dumpPages args
          "rip" -> rewritePages args
          "reconstruct" -> rewritePackets args
          "countrw" -> countrwPages args
          "dumpraw" -> dumpRawPages args
          "merge" -> mergePages args
          _ -> helpCommands
