module Main where

import System.Exit
import System.IO

import Control.Monad.Reader
import Control.Monad
import Control.Exception

import Network.HTTP (rspBody)
import Network.HTTP.UserAgent as UA

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Text.Printf

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Data.Maybe (fromJust)

import Codec.Container.Ogg.Chain
import Codec.Container.Ogg.Chop
import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.ListMerge
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.RawPage
import Codec.Container.Ogg.Skeleton
import Codec.Container.Ogg.Timestamp
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
    subCategory :: String,
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
               chopSub,
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
    contentTypeCfg :: Maybe ContentType,
    outputCfg :: Maybe String,
    startCfg :: Maybe Timestamp,
    endCfg :: Maybe Timestamp,
    files :: [FilePath]
  }

dftConfig =
  Config {
    contentTypeCfg = Nothing,
    outputCfg = Nothing,
    startCfg = Nothing,
    endCfg = Nothing,
    files = ["-"]
  }

-- Options available for subcommands
--
data Option = Help
            | ContentTypeOpt String
            | OutputOpt String
            | StartOpt String
            | EndOpt String
            deriving Eq

options :: [OptDescr Option]
options = [ Option ['h', '?'] ["help"] (NoArg Help)
              "Display this help and exit"
          , Option ['c']      ["content-type"] (ReqArg ContentTypeOpt "Content-Type")
              "Select the logical bitstreams for a specified content type"
          , Option ['s']      ["start"] (ReqArg StartOpt "Timestamp")
              "Specify a start time"
          , Option ['e']      ["end"] (ReqArg EndOpt "Timestamp")
              "Specify an end time"
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
    processOneOption config (ContentTypeOpt ctype) = do
      -- let c = catchRead "Invalid content type" ctype
      let c = parseType ctype
      return $ config {contentTypeCfg = c}
    processOneOption config (OutputOpt output) = do
      return $ config {outputCfg = Just output}
    processOneOption config (StartOpt start) = do
      let s = catchRead "Invalid start time" start
      return $ config {startCfg = Just s}
    processOneOption config (EndOpt end) = do
      let e = catchRead "Invalid end time" end
      return $ config {endCfg = Just e}

catchRead :: (Read a) => String -> String -> a
catchRead msg x = case reads x of
    [] -> error (msg ++ ": " ++ x)
    [(ms,mt)] -> ms

------------------------------------------------------------
-- Hot: actions for getting Ogg data from the input files
--

-- get the named contents as a bytestring
open :: String -> IO L.ByteString
open f = case (isPrefixOf "http://" f) of
    True -> do
      rsp <- UA.get f
      return $ rspBody rsp
    False -> do
      h <- openFile f ReadMode
      i <- L.hGetContents h
      return i

-- rawpages is used only by "hogg dumpraw"
rawpages :: Hot [[OggRawPage]]
rawpages = do
    filenames <- asks hotFilenames
    inputs <- mapM (liftIO . open) filenames
    return $ map rawPageScan inputs

-- chains, tracks, pages, packets
allChains :: Hot [[OggChain]]
allChains = do
    filenames <- asks hotFilenames
    inputs <- mapM (liftIO . open) filenames
    return $ map chainScan inputs

-- All tracks, from all files, matching the given criteria
tracks :: Hot [[[OggTrack]]]
tracks = chainMatch chainTracks

-- All pages, from all files, matching the given criteria
pages :: Hot [[[OggPage]]]
pages = chainMatch chainPages

-- All packets, from all files, matching the given criteria
packets :: Hot [[[OggPacket]]]
packets = chainMatch chainPackets

-- All chains, from all files, with elements matching the given criteria
chains :: Hot [[OggChain]]
chains = chainMatchM chainFilter

-- | A generic function to run a Hot function over all chains
chainMatchM :: (OggChain -> Hot a) -> Hot [[a]]
chainMatchM f = do
    c <- allChains
    let all = map (mapM f) c
    sequence all

-- | Filter all elements of a chain by the given criteria
chainFilter :: OggChain -> Hot OggChain
chainFilter (OggChain t gs ps) = do
    t' <- mType t
    gs' <- mType gs
    ps' <- mType ps
    return $ OggChain t' gs' ps'

-- | A generic function to pull a list of things from a chain
chainMatch :: (ContentTyped a) => (OggChain -> [a]) -> Hot [[[a]]]
chainMatch f = do
    c <- allChains
    let all = map (map f) c
    sequence $ (map (mapM mType)) all

-- | Filter a ContentTyped list by the given content type
mType :: (ContentTyped a) => [a] -> Hot [a]
mType xs = do
    config <- asks hotConfig
    return $ case (contentTypeCfg config) of
      Nothing -> xs
      Just t -> filter (contentTypeIs t) xs

-- | Apply matchRange to all the inner inner lists
matchRange :: (Timestampable a) => [[[a]]] -> Hot [[[a]]]
matchRange all = sequence $ (map (mapM mRange)) all

-- | Filter a Timestampable list by the given time range
mRange :: (Timestampable a) => [a] -> Hot [a]
mRange xs = do
    config <- asks hotConfig
    return $ between (startCfg config) (endCfg config) xs

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
reportPerFile [r] = outputC r -- Don't add banners if only one file to report
reportPerFile l = do
    filenames <- asks hotFilenames
    let fHeader f = C.pack $ printf "Filename: %s\n\n" f
    let fText = zip (map fHeader filenames) l
    let sep = C.pack $ replicate 60 '-' ++ "\n"
    let banner (f,t) = C.concat [sep,f,t]
    outputC $ C.concat $ map banner fText

-- Output binary data
outputPerFile :: [L.ByteString] -> Hot ()
outputPerFile l = outputL $ L.concat l

-- Place a marker betwen the reports for each chain
reportPerChain :: [C.ByteString] -> C.ByteString
reportPerChain l = C.concat $ intersperse (C.pack (chainMarker++"\n\n")) l
  where
    -- chainMarker = "><> New Chain <><><><"
    chainMarker = "><><><> New Chain ><>"

-- Concat the output for each chain
outputPerChain :: [L.ByteString] -> L.ByteString
outputPerChain = L.concat

------------------------------------------------------------
-- info
--

infoSub :: SubCommand
infoSub = SubCommand "info" info
    "Reporting" "Display information about the file and its bitstreams"

info :: Hot ()
info = do
    matchTracks <- tracks
    let t = \x -> show x ++ "\n" -- Add a newline after each track's info
    let i = \x -> reportPerChain $ map (C.concat . map (C.pack . t)) x
    reportPerFile $ map i matchTracks

------------------------------------------------------------
-- dumpPackets (dump)
--

dumpPacketsSub :: SubCommand
dumpPacketsSub = SubCommand "dump" dumpPackets
    "Reporting" "Hexdump packets of an Ogg file"

dumpPackets :: Hot ()
dumpPackets = do
    matchPackets <- matchRange =<< packets
    let d = \x -> reportPerChain $ map (C.concat . map packetToBS) x
    reportPerFile $ map d matchPackets

------------------------------------------------------------
-- countPackets (packetcount)
--

countPacketsSub :: SubCommand
countPacketsSub = SubCommand "packetcount" countPackets
    "Testing" "Count packets of an Ogg file" 

countPackets :: Hot ()
countPackets = do
    matchPackets <- matchRange =<< packets
    let c = \x -> C.pack $ show (length x) ++ " packets\n"
    let r = \x -> reportPerChain $ map c x
    reportPerFile $ map r matchPackets

------------------------------------------------------------
-- rewritePages (rip)
--

rewritePagesSub :: SubCommand
rewritePagesSub = SubCommand "rip" rewritePages
    "Extraction" "Rip selected logical bistreams from an Ogg file (default: all)"

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
    "Extraction" "Reconstruct an Ogg file by doing a full packet demux"

rewritePackets :: Hot ()
rewritePackets = do
    matchPackets <- packets
    let r = \x -> L.concat $ map pageWrite (packetsToPages x)
    let r2 = \x -> outputPerChain $ map r x
    outputPerFile $ map r2 matchPackets

------------------------------------------------------------
-- chop
--

chopSub :: SubCommand
chopSub = SubCommand "chop" chopPages
  "Editing" "Extract a section (specify start and/or end time)"

chopPages :: Hot ()
chopPages = do
    config <- asks hotConfig
    matchChains <- chains
    chopChains <- mapM (mapM (chopRange config)) matchChains
    let c = \x -> L.concat $ map pageWrite (chainPages x)
    let c2 = \x -> outputPerChain $ map c x
    outputPerFile $ map c2 chopChains

chopRange :: Config -> OggChain -> Hot OggChain
chopRange c@(Config _ _ start end _) xs = liftIO $ chopWithSkel start end xs

-- TODO: implement an option for the following ...
-- To make with no skeleton bitstream:
-- chopRange c@(Config _ _ start end _) xs = liftIO $ chop start end xs

------------------------------------------------------------
-- addSkel (addskel)
--

addSkelSub :: SubCommand
addSkelSub = SubCommand "addskel" addSkel
  "Editing" "Write a Skeleton logical bitstream"

addSkel :: Hot ()
addSkel = do
    c <- allChains
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
    "Testing" "Rewrite via packets and display a count of pages produced"

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
    "Testing" "Count pages of an Ogg file" 

countPages :: Hot ()
countPages = do
    matchPages <- matchRange =<< pages
    let c = \x -> C.pack $ printf "%d pages\n" (length x)
    let r = \x -> reportPerChain $ map c x
    reportPerFile $ map r matchPages

------------------------------------------------------------
-- dumpPages (pagedump)
--

dumpPagesSub :: SubCommand
dumpPagesSub = SubCommand "pagedump" dumpPages
    "Reporting" "Display page structure of an Ogg file"

dumpPages :: Hot ()
dumpPages = do
    matchPages <- matchRange =<< pages
    let d = \x -> C.concat $ map (C.pack . show) x
    let r = \x -> reportPerChain $ map d x
    reportPerFile $ map r matchPages

------------------------------------------------------------
-- mergePages (merge)
--

mergePagesSub :: SubCommand
mergePagesSub = SubCommand "merge" mergePages
    "Editing" "Merge, interleaving pages in order of presentation time"

mergePages :: Hot ()
mergePages = do
    matchPages <- pages
    -- XXX: only use the first chain of each input file. Using subsequent
    -- chains won't work anyway unless corresponding chains in each file are
    -- of identical duration.
    let firstChainPages = map head matchPages
    outputL $ L.concat $ map pageWrite $ listMerge firstChainPages

------------------------------------------------------------
-- dumpRawPages (dumpraw)
--

dumpRawPagesSub :: SubCommand
dumpRawPagesSub = SubCommand "dumpraw" dumpRawPages
    "Reporting" "Dump raw (unparsed) page data"

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
  "Commands" "Display help for a specific subcommand"

help :: Hot ()
help = do
    args <- asks hotFilenames
    outputC $ C.concat $ map C.pack $ longHelp args

longHelp :: [String] -> [String]
-- | "hogg help" with no arguments: Give a list of all subcommands
longHelp [] =
    ["Usage: hogg <subcommand> [options] filename ...\n\n"] ++
    map categoryHelp ["Commands", "Reporting", "Extraction", "Editing"] ++
    -- map categoryHelp ["Testing"] ++
    ["Please report bugs to <ogg-dev@xiph.org>\n"]

-- | "hogg help command": Give command-specific help
longHelp (command:_) = contextHelp command m
  where m = filter (\x -> subName x == command) subCommands

-- | Provide synopses for a specific category of commands
categoryHelp :: String -> String
categoryHelp c = c ++ ":\n" ++ concat (map itemHelp items) ++ "\n"
  where items = filter (\x -> subCategory x == c) subCommands
        itemHelp i = printf "  %-14s%s\n" (subName i) (subSynopsis i)

-- | Provide detailed help for a specific command
contextHelp command [] = longHelp [] ++ contextError
  where contextError = ["\n*** \"" ++ command ++ "\": Unknown command.\n"]
contextHelp command (item:_) = synopsis ++ usage ++ ["\n" ++ optionsHelp command]
  where usage = ["Usage: hogg " ++ command ++ " [options] filename\n"]
        synopsis = [command ++ ": " ++ subSynopsis item ++ "\n"]

-- | Provide usage information [for a specific command]
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
