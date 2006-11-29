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
import Ogg.ListMerge
import Ogg.Page
import Ogg.Packet
import Ogg.RawPage
import Ogg.Skeleton
import Ogg.Track


------------------------------------------------------------
-- Subcommands
--

data SubCommand =
  SubCommand {
    subName :: String,
    subMethod :: [String] -> IO (),
    subSynopsis :: String
  }

subCommands :: [SubCommand]
subCommands = [
               infoSub,
               dumpPacketsSub,
               dumpPagesSub,
               dumpRawPagesSub,
               countPagesSub,
               addSkelSub,
               rewritePagesSub,
               mergePagesSub,
               rewritePacketsSub,
               countrwPagesSub,
               countPacketsSub
              ]

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

getChains :: FilePath -> IO [OggChain]
getChains filename = do
  handle <- openFile filename ReadMode
  input <- L.hGetContents handle
  return $ chainScan input

getTracks :: FilePath -> IO [OggTrack]
getTracks filename = do
    chains <- getChains filename
    return $ chainTracks $ head chains

getRawPages :: FilePath -> IO [OggRawPage]
getRawPages filename = do
    handle <- openFile filename ReadMode
    input <- L.hGetContents handle
    return $ rawPageScan input

getPages :: FilePath -> IO [OggPage]
getPages filename = do
    chains <- getChains filename
    return $ chainPages $ head chains

getPackets :: FilePath -> IO [OggPacket]
getPackets filename = do
    chains <- {-# SCC "getChains" #-}getChains filename
    return $ chainPackets $ head chains

trackMatch :: Maybe OggType -> [OggTrack] -> [OggTrack]
trackMatch Nothing ts = ts
trackMatch (Just t) ts = filter (trackIsType t) ts

pageMatch :: Maybe OggType -> [OggPage] -> [OggPage]
pageMatch Nothing gs = gs
pageMatch (Just t) gs = filter (pageIsType t) gs

packetMatch :: Maybe OggType -> [OggPacket] -> [OggPacket]
packetMatch Nothing ps = ps
packetMatch (Just t) ps = filter (packetIsType t) ps

mTracks :: Config -> String -> IO [OggTrack]
mTracks config filename = do
    let ctype = parseType $ contentTypeCfg config
    allTracks <- getTracks filename
    return $ trackMatch ctype allTracks

mRawPages :: Config -> String -> IO [OggRawPage]
mRawPages config filename = do
    -- let ctype = parseType $ contentTypeCfg config
    allRawPages <- getRawPages filename
    return allRawPages

mPages :: Config -> String -> IO [OggPage]
mPages config filename = do
    let ctype = parseType $ contentTypeCfg config
    allPages <- getPages filename
    return $ pageMatch ctype allPages

mPackets :: Config -> String -> IO [OggPacket]
mPackets config filename = do
    let ctype = parseType $ contentTypeCfg config
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

------------------------------------------------------------
-- info
--

infoSub :: SubCommand
infoSub = SubCommand "info" info
    "Display information about the file and its bitstreams"

info :: [String] -> IO ()
info args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchTracks <- mTracks config filename
    outputC config $ C.concat $ map (C.pack . show) matchTracks

------------------------------------------------------------
-- dumpPackets (dump)
--

dumpPacketsSub :: SubCommand
dumpPacketsSub = SubCommand "dump" dumpPackets
    "Hexdump packets of an Ogg file"

dumpPackets :: [String] -> IO ()
dumpPackets args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPackets <- {-# SCC "matchPackets" #-}mPackets config filename
    outputC config $ C.concat $ map packetToBS matchPackets

------------------------------------------------------------
-- countPackets (packetcount)
--

countPacketsSub :: SubCommand
countPacketsSub = SubCommand "packetcount" countPackets
    "Count packets of an Ogg file" 

countPackets :: [String] -> IO ()
countPackets args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPackets <- mPackets config filename
    outputS config $ show (length matchPackets) ++ " packets\n"

------------------------------------------------------------
-- rewritePages (rip)
--

rewritePagesSub :: SubCommand
rewritePagesSub = SubCommand "rip" rewritePages
    "Rip selected logical bistreams from an Ogg file (default: all)"

rewritePages :: [String] -> IO ()
rewritePages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mPages config filename
    outputL config $ L.concat (map pageWrite matchPages)

------------------------------------------------------------
-- rewritePackets (reconstruct)
--

rewritePacketsSub :: SubCommand
rewritePacketsSub = SubCommand "reconstruct" rewritePackets
    "Reconstruct an Ogg file by doing a full packet demux"

rewritePackets :: [String] -> IO ()
rewritePackets args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPackets <- mPackets config filename
    outputL config $ L.concat (map pageWrite (packetsToPages matchPackets))

------------------------------------------------------------
-- addSkel (addskel)
--

addSkelSub :: SubCommand
addSkelSub = SubCommand "addskel" addSkel
  "Write a Skeleton logical bitstream"

addSkel :: [String] -> IO ()
addSkel args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    chains <- getChains filename
    skelChain <- chainAddSkeleton $ head chains
    outputL config $ L.concat (map pageWrite (chainPages skelChain))
    -- let matchPackets = chainPackets skelChain
    -- outputC config $ C.concat $ map packetToBS matchPackets
  
------------------------------------------------------------
-- countrwPages (countrw)
--

countrwPagesSub :: SubCommand
countrwPagesSub = SubCommand "countrw" countrwPages
    "Rewrite an Ogg file via packets and display a count"

countrwPages :: [String] -> IO ()
countrwPages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mPages config filename
    outputS config $ show $ length (packetsToPages (pagesToPackets matchPages))

------------------------------------------------------------
-- countPages (pagecount)
--

countPagesSub :: SubCommand
countPagesSub = SubCommand "pagecount" countPages
    "Count pages of an Ogg file" 

countPages :: [String] -> IO ()
countPages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mPages config filename
    outputS config $ (show $ length matchPages) ++ " pages\n"

------------------------------------------------------------
-- dumpPages (pagedump)
--

dumpPagesSub :: SubCommand
dumpPagesSub = SubCommand "pagedump" dumpPages
    "Display page structure of an Ogg file"

dumpPages :: [String] -> IO ()
dumpPages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mPages config filename
    outputC config $ C.concat $ map (C.pack . show) matchPages

------------------------------------------------------------
-- mergePages (merge)
--

mergePagesSub :: SubCommand
mergePagesSub = SubCommand "merge" mergePages
    "Merge, interleaving pages in order of presentation time"
  
mergePages :: [String] -> IO ()
mergePages args = do
    (config, filenames) <- processArgs args
    matchPages <- mapM (mPages config) filenames
    outputL config $ L.concat $ map pageWrite $ listMerge matchPages

------------------------------------------------------------
-- dumpRawPages (dumpraw)
--

dumpRawPagesSub :: SubCommand
dumpRawPagesSub = SubCommand "dumpraw" dumpRawPages
    "Dump raw (unparsed) page data"

dumpRawPages :: [String] -> IO ()
dumpRawPages args = do
    (config, filenames) <- processArgs args
    let filename = head filenames
    matchPages <- mRawPages config filename
    outputC config $ C.concat $ map (C.pack . show) matchPages

------------------------------------------------------------
-- shortHelp
--

shortHelp :: [String] -> IO ()
shortHelp args = do
    (config, commands) <- processArgs args
    outputC config $ C.concat $ map C.pack $ longHelp commands

longHelp :: [String] -> [String]
-- | "hogg help" with no arguments: Give a list of all subcommands
longHelp [] =
    ["Usage: hogg <subcommand> [options] filename ...\n\n",
     "Available subcommands:\n"] ++
    map itemHelp subCommands ++
    ["\nPlease report bugs to <ogg-dev@xiph.org>\n"]
    where
        itemHelp i = printf "  %-14s%s\n" (subName i) (subSynopsis i)

-- | "hogg help <command>": Give command-specific help
longHelp (command:_) = contextHelp command m
  where m = filter (\x -> subName x == command) subCommands

contextHelp command [] = longHelp [] ++ contextError
  where contextError = ["\n*** \"" ++ command ++ "\": Unknown command.\n"]
contextHelp command (item:_) = synopsis ++ usage ++ ["\n" ++ optionsHelp command]
  where usage = ["Usage: hogg " ++ command ++ " [options] filename\n"]
        synopsis = [command ++ ": " ++ subSynopsis item ++ "\n"]

optionsHelp command = usageInfo "Options:" options

------------------------------------------------------------
-- main
--

helpStrings = ["--help", "-h", "-?"]

isHelp :: String -> Bool
isHelp x = elem x helpStrings

main :: IO ()
main = do
    allArgs <- getArgs
    when (any isHelp allArgs) $ do
      shortHelp $ filter (not . isHelp) allArgs
      exitWith ExitSuccess
    case allArgs of
      []             -> shortHelp []
      (command:args) -> do
        act args $ filter (\x -> subName x == command) subCommands
    where
      act a [] = shortHelp a
      act a (s:_) = (subMethod s) a
