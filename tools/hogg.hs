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
import Data.List

import Codec.Container.Ogg.Chain
import Codec.Container.Ogg.ListMerge
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.RawPage
import Codec.Container.Ogg.Skeleton
import Codec.Container.Ogg.Track

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

------------------------------------------------------------
-- Hot: actions for getting Ogg data from the input files
--

-- rawpages is used only by "hogg dumpraw"
rawpages :: Hot [[OggRawPage]]
rawpages = do
    filenames <- asks hotFilenames
    handles <- mapM ioOpenReadFile filenames
    inputs <- mapM ioGetContents handles
    return $ map rawPageScan inputs
  where
    ioOpenReadFile f = liftIO $ openFile f ReadMode
    ioGetContents = liftIO . L.hGetContents

-- chains, tracks, pages, packets
chains :: Hot [[OggChain]]
chains = do
    filenames <- asks hotFilenames
    handles <- mapM ioOpenReadFile filenames
    inputs <- mapM ioGetContents handles
    return $ map chainScan inputs
  where
    ioOpenReadFile f = liftIO $ openFile f ReadMode
    ioGetContents = liftIO . L.hGetContents

-- All tracks, from all files, matching the given criteria
tracks :: Hot [[[OggTrack]]]
tracks = do
    c <- chains
    let allTracks = map (map chainTracks) c
    config <- asks hotConfig
    let ctype = parseType $ contentTypeCfg config
    return $ map (map (trackMatch ctype)) allTracks
  where
    trackMatch :: Maybe OggType -> [OggTrack] -> [OggTrack]
    trackMatch Nothing ts = ts
    trackMatch (Just t) ts = filter (trackIsType t) ts

-- All pages, from all files, matching the given criteria
pages :: Hot [[[OggPage]]]
pages = do
    config <- asks hotConfig
    let ctype = parseType $ contentTypeCfg config
    c <- chains
    let allPages = map (map chainPages) c
    config <- asks hotConfig
    let ctype = parseType $ contentTypeCfg config
    return $ map (map (pageMatch ctype)) allPages
  where
    pageMatch :: Maybe OggType -> [OggPage] -> [OggPage]
    pageMatch Nothing gs = gs
    pageMatch (Just t) gs = filter (pageIsType t) gs

-- All packets, from all files, matching the given criteria
packets :: Hot [[[OggPacket]]]
packets = do
    c <- chains
    let allPackets = map (map chainPackets) c
    config <- asks hotConfig
    let ctype = parseType $ contentTypeCfg config
    return $ map (map (packetMatch ctype)) allPackets
  where
    packetMatch :: Maybe OggType -> [OggPacket] -> [OggPacket]
    packetMatch Nothing ps = ps
    packetMatch (Just t) ps = filter (packetIsType t) ps

------------------------------------------------------------
-- Output helpers
--

-- The output handle; stdout unless otherwise specified
outputHandle :: Config -> IO Handle
outputHandle config =
    maybe (evaluate stdout) (\f -> openBinaryFile f WriteMode) (outputCfg config)

-- Output a Data.ByteString.Lazy.Char8
outputC :: C.ByteString -> Hot ()
outputC bs = do
    config <- asks hotConfig
    h <- liftIO $ outputHandle config
    liftIO $ C.hPut h bs
    liftIO $ hClose h

-- Output a Data.ByteString.Lazy
outputL ::  L.ByteString -> Hot ()
outputL bs = do
    config <- asks hotConfig
    h <- liftIO $ outputHandle config
    liftIO $ L.hPut h bs
    liftIO $ hClose h

-- Output with a text banner per input file
reportPerFile :: [C.ByteString] -> Hot ()
reportPerFile l = do
    filenames <- asks hotFilenames
    let fHeader f = C.pack $ printf "Filename: %s\n" f
    let fText = zip (map fHeader filenames) l
    let sep = C.pack $ replicate 60 '-' ++ "\n"
    let banner (f,t) = C.concat [sep,f,t]
    outputC $ C.concat $ map banner fText

-- Output binary data
outputPerFile :: [L.ByteString] -> Hot ()
outputPerFile l = outputL $ L.concat l

-- Place a marker betwen the reports for each chain
reportPerChain :: [C.ByteString] -> C.ByteString
reportPerChain l = C.concat $ intersperse (C.pack ">>> New Chain:\n") l

-- Concat the output for each chain
outputPerChain :: [L.ByteString] -> L.ByteString
outputPerChain = L.concat

------------------------------------------------------------
-- info
--

infoSub :: SubCommand
infoSub = SubCommand "info" info
    "Display information about the file and its bitstreams"

info :: Hot ()
info = do
    matchTracks <- tracks
    let i = \x -> reportPerChain $ map (C.concat . map (C.pack . show)) x
    reportPerFile $ map i matchTracks

------------------------------------------------------------
-- dumpPackets (dump)
--

dumpPacketsSub :: SubCommand
dumpPacketsSub = SubCommand "dump" dumpPackets
    "Hexdump packets of an Ogg file"

dumpPackets :: Hot ()
dumpPackets = do
    matchPackets <- packets
    let d = \x -> reportPerChain $ map (C.concat . map packetToBS) x
    reportPerFile $ map d matchPackets

------------------------------------------------------------
-- countPackets (packetcount)
--

countPacketsSub :: SubCommand
countPacketsSub = SubCommand "packetcount" countPackets
    "Count packets of an Ogg file" 

countPackets :: Hot ()
countPackets = do
    matchPackets <- packets
    let c = \x -> C.pack $ show (length x) ++ " packets\n"
    let r = \x -> reportPerChain $ map c x
    reportPerFile $ map r matchPackets

------------------------------------------------------------
-- rewritePages (rip)
--

rewritePagesSub :: SubCommand
rewritePagesSub = SubCommand "rip" rewritePages
    "Rip selected logical bistreams from an Ogg file (default: all)"

rewritePages :: Hot ()
rewritePages = do
    matchPages <- pages
    let r = \x -> L.concat $ map pageWrite x
    let r2 = \x -> outputPerChain $ map r x
    outputPerFile $ map r2 matchPages

------------------------------------------------------------
-- rewritePackets (reconstruct)
--

rewritePacketsSub :: SubCommand
rewritePacketsSub = SubCommand "reconstruct" rewritePackets
    "Reconstruct an Ogg file by doing a full packet demux"

rewritePackets :: Hot ()
rewritePackets = do
    matchPackets <- packets
    let r = \x -> L.concat $ map pageWrite (packetsToPages x)
    let r2 = \x -> outputPerChain $ map r x
    outputPerFile $ map r2 matchPackets

------------------------------------------------------------
-- addSkel (addskel)
--

addSkelSub :: SubCommand
addSkelSub = SubCommand "addskel" addSkel
  "Write a Skeleton logical bitstream"

addSkel :: Hot ()
addSkel = do
    c <- chains
    skels <- mapM (mapM ioAddSkeleton) c
    let s = \x -> L.concat $ map pageWrite (chainPages x)
    let s2 = \x -> outputPerChain $ map s x
    outputPerFile $ map s2 skels
  where
    ioAddSkeleton x = liftIO $ chainAddSkeleton x
  
------------------------------------------------------------
-- countrwPages (countrw)
--

countrwPagesSub :: SubCommand
countrwPagesSub = SubCommand "countrw" countrwPages
    "Rewrite via packets and display a count of pages produced"

countrwPages :: Hot ()
countrwPages = do
    matchPages <- pages
    let c = \x -> C.pack $ printf "%d pages\n" (length (packetsToPages (pagesToPackets x)))
    let r = \x -> reportPerChain $ map c x
    reportPerFile $ map r matchPages

------------------------------------------------------------
-- countPages (pagecount)
--

countPagesSub :: SubCommand
countPagesSub = SubCommand "pagecount" countPages
    "Count pages of an Ogg file" 

countPages :: Hot ()
countPages = do
    matchPages <- pages
    let c = \x -> C.pack $ printf "%d pages\n" (length x)
    let r = \x -> reportPerChain $ map c x
    reportPerFile $ map r matchPages

------------------------------------------------------------
-- dumpPages (pagedump)
--

dumpPagesSub :: SubCommand
dumpPagesSub = SubCommand "pagedump" dumpPages
    "Display page structure of an Ogg file"

dumpPages :: Hot ()
dumpPages = do
    matchPages <- pages
    let d = \x -> C.concat $ map (C.pack . show) x
    let r = \x -> reportPerChain $ map d x
    reportPerFile $ map r matchPages

------------------------------------------------------------
-- mergePages (merge)
--

mergePagesSub :: SubCommand
mergePagesSub = SubCommand "merge" mergePages
    "Merge, interleaving pages in order of presentation time"

mergePages :: Hot ()
mergePages = do
    matchPages <- pages
    -- XXX: only use the first chain of each input file. Using subsequent
    -- chains won't work anyway unless corresponding chains in each file are
    -- of identical duration.
    let firstChainPages = map (map head) matchPages
    outputL $ L.concat $ map pageWrite $ listMerge firstChainPages

------------------------------------------------------------
-- dumpRawPages (dumpraw)
--

dumpRawPagesSub :: SubCommand
dumpRawPagesSub = SubCommand "dumpraw" dumpRawPages
    "Dump raw (unparsed) page data"

dumpRawPages :: Hot ()
dumpRawPages = do
    matchPages <- rawpages
    let d = \x -> C.concat $ map (C.pack . show) matchPages
    reportPerFile $ map d matchPages

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

-- | "hogg help command": Give command-specific help
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
