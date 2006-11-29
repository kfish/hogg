module Main where

import System.Exit
import System.IO

import Control.Monad.Reader
import Control.Monad
import Control.Exception

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

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
--  HOggTool datatype
--

data HOggTool =
  HOggTool {
    hotConfig :: Config,
    hotFilenames :: [String]
  }

type Hot = ReaderT HOggTool IO

------------------------------------------------------------
-- Subcommands
--

data SubCommand =
  SubCommand {
    subName :: String,
    subMethod :: Hot (),
    subSynopsis :: String
  }

subCommands :: [SubCommand]
subCommands = [
               infoSub,
               dumpPacketsSub,
               dumpPagesSub,
               dumpRawPagesSub,
               rewritePagesSub,
               rewritePacketsSub,
               mergePagesSub,
               addSkelSub,
               countPacketsSub,
               countrwPagesSub,
               countPagesSub,
               helpSub
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

getChains :: FilePath -> Hot [OggChain]
getChains filename = do
  handle <- liftIO $ openFile filename ReadMode
  input <- liftIO $ L.hGetContents handle
  return $ chainScan input

getTracks :: FilePath -> Hot [OggTrack]
getTracks filename = do
    chains <- getChains filename
    return $ chainTracks $ head chains

getRawPages :: FilePath -> Hot [OggRawPage]
getRawPages filename = do
    handle <- liftIO $ openFile filename ReadMode
    input <- liftIO $ L.hGetContents handle
    return $ rawPageScan input

getPages :: FilePath -> Hot [OggPage]
getPages filename = do
    chains <- getChains filename
    return $ chainPages $ head chains

getPackets :: FilePath -> Hot [OggPacket]
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

mTracks :: String -> Hot [OggTrack]
mTracks filename = do
    config <- asks hotConfig
    let ctype = parseType $ contentTypeCfg config
    allTracks <- getTracks filename
    return $ trackMatch ctype allTracks

mRawPages :: String -> Hot [OggRawPage]
mRawPages filename = do
    -- config <- asks hotConfig
    -- let ctype = parseType $ contentTypeCfg config
    allRawPages <- getRawPages filename
    return allRawPages

mPages :: String -> Hot [OggPage]
mPages filename = do
    config <- asks hotConfig
    let ctype = parseType $ contentTypeCfg config
    allPages <- getPages filename
    return $ pageMatch ctype allPages

mPackets :: String -> Hot [OggPacket]
mPackets filename = do
    config <- asks hotConfig
    let ctype = parseType $ contentTypeCfg config
    allPackets <- {-# SCC "getPackets" #-}getPackets filename
    return $ packetMatch ctype allPackets

outputHandle :: Config -> IO Handle
outputHandle config =
    maybe (evaluate stdout) (\f -> openBinaryFile f WriteMode) (outputCfg config)

outputS :: String -> Hot ()
outputS s = do
    config <- asks hotConfig
    h <- liftIO $ outputHandle config
    liftIO $ hPutStr h s
    liftIO $ hClose h

outputC :: C.ByteString -> Hot ()
outputC bs = do
    config <- asks hotConfig
    h <- liftIO $ outputHandle config
    liftIO $ C.hPut h bs
    liftIO $ hClose h

outputL ::  L.ByteString -> Hot ()
outputL bs = do
    config <- asks hotConfig
    h <- liftIO $ outputHandle config
    liftIO $ L.hPut h bs
    liftIO $ hClose h

------------------------------------------------------------
-- info
--

infoSub :: SubCommand
infoSub = SubCommand "info" info
    "Display information about the file and its bitstreams"

info :: Hot ()
info = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchTracks <- mTracks filename
    outputC $ C.concat $ map (C.pack . show) matchTracks

------------------------------------------------------------
-- dumpPackets (dump)
--

dumpPacketsSub :: SubCommand
dumpPacketsSub = SubCommand "dump" dumpPackets
    "Hexdump packets of an Ogg file"

dumpPackets :: Hot ()
dumpPackets = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchPackets <- {-# SCC "matchPackets" #-}mPackets filename
    outputC $ C.concat $ map packetToBS matchPackets

------------------------------------------------------------
-- countPackets (packetcount)
--

countPacketsSub :: SubCommand
countPacketsSub = SubCommand "packetcount" countPackets
    "Count packets of an Ogg file" 

countPackets :: Hot ()
countPackets = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchPackets <- mPackets filename
    outputS $ show (length matchPackets) ++ " packets\n"

------------------------------------------------------------
-- rewritePages (rip)
--

rewritePagesSub :: SubCommand
rewritePagesSub = SubCommand "rip" rewritePages
    "Rip selected logical bistreams from an Ogg file (default: all)"

rewritePages :: Hot ()
rewritePages = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchPages <- mPages filename
    outputL $ L.concat (map pageWrite matchPages)

------------------------------------------------------------
-- rewritePackets (reconstruct)
--

rewritePacketsSub :: SubCommand
rewritePacketsSub = SubCommand "reconstruct" rewritePackets
    "Reconstruct an Ogg file by doing a full packet demux"

rewritePackets :: Hot ()
rewritePackets = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchPackets <- mPackets filename
    outputL $ L.concat (map pageWrite (packetsToPages matchPackets))

------------------------------------------------------------
-- addSkel (addskel)
--

addSkelSub :: SubCommand
addSkelSub = SubCommand "addskel" addSkel
  "Write a Skeleton logical bitstream"

addSkel :: Hot ()
addSkel = do
    filenames <- asks hotFilenames
    let filename = head filenames
    chains <- getChains filename
    skelChain <- liftIO $ chainAddSkeleton $ head chains
    outputL $ L.concat (map pageWrite (chainPages skelChain))
    -- let matchPackets = chainPackets skelChain
    -- outputC config $ C.concat $ map packetToBS matchPackets
  
------------------------------------------------------------
-- countrwPages (countrw)
--

countrwPagesSub :: SubCommand
countrwPagesSub = SubCommand "countrw" countrwPages
    "Rewrite an Ogg file via packets and display a count"

countrwPages :: Hot ()
countrwPages = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchPages <- mPages filename
    outputS $ show $ length (packetsToPages (pagesToPackets matchPages))

------------------------------------------------------------
-- countPages (pagecount)
--

countPagesSub :: SubCommand
countPagesSub = SubCommand "pagecount" countPages
    "Count pages of an Ogg file" 

countPages :: Hot ()
countPages = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchPages <- mPages filename
    outputS $ (show $ length matchPages) ++ " pages\n"

------------------------------------------------------------
-- dumpPages (pagedump)
--

dumpPagesSub :: SubCommand
dumpPagesSub = SubCommand "pagedump" dumpPages
    "Display page structure of an Ogg file"

dumpPages :: Hot ()
dumpPages = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchPages <- mPages filename
    outputC $ C.concat $ map (C.pack . show) matchPages

------------------------------------------------------------
-- mergePages (merge)
--

mergePagesSub :: SubCommand
mergePagesSub = SubCommand "merge" mergePages
    "Merge, interleaving pages in order of presentation time"
  
mergePages :: Hot ()
mergePages = do
    filenames <- asks hotFilenames
    matchPages <- mapM mPages filenames
    outputL $ L.concat $ map pageWrite $ listMerge matchPages

------------------------------------------------------------
-- dumpRawPages (dumpraw)
--

dumpRawPagesSub :: SubCommand
dumpRawPagesSub = SubCommand "dumpraw" dumpRawPages
    "Dump raw (unparsed) page data"

dumpRawPages :: Hot ()
dumpRawPages = do
    filenames <- asks hotFilenames
    let filename = head filenames
    matchPages <- mRawPages filename
    outputC $ C.concat $ map (C.pack . show) matchPages

------------------------------------------------------------
-- help
--

helpSub :: SubCommand
helpSub = SubCommand "help" help
  "Display help information"

help :: Hot ()
help = do
    args <- asks hotFilenames
    outputC $ C.concat $ map C.pack $ longHelp args

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

initTool :: [String] -> IO HOggTool
initTool args = do
    (config, filenames) <- processArgs args
    return $ HOggTool config filenames

main :: IO ()
main = do
    allArgs <- getArgs
    when (any isHelp allArgs) $ showHelp allArgs
    handleSubCommand allArgs

showHelp :: [String] -> IO ()
showHelp allArgs = -- bracket init1 finish1 loop1
  init1 >>= loop1
  where
    init1 = initTool $ filter (not . isHelp) allArgs
    loop1 st = runReaderT run1 st
    run1 = help
    finish1 = exitWith ExitSuccess

handleSubCommand :: [String] -> IO ()
handleSubCommand [] = -- bracket (initTool []) finish loop0
  (initTool []) >>= loop0
  where
    finish = exitWith ExitSuccess
    loop0 st = runReaderT help st
    
handleSubCommand (command:args) = -- bracket (initTool args) finish loop1
  (initTool args) >>= loop1
  where
    finish = exitWith ExitSuccess
    loop1 st = runReaderT run st
    run = act $ filter (\x -> subName x == command) subCommands
    act [] = help
    act (s:_) = (subMethod s)
