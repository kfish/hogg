{-# OPTIONS_GHC -cpp #-}
module Main where

import System.Exit
import System.IO

import Control.Monad.Reader
import Control.Monad
import Control.Exception

#ifdef USE_HTTP
import Network.HTTP (rspBody)
import Network.HTTP.UserAgent as UA
#endif

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Text.Printf

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List hiding (sort)

import Codec.Container.Ogg.Chain
import Codec.Container.Ogg.Chop
import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.RawPage
import Codec.Container.Ogg.Sort
import Codec.Container.Ogg.Timestamp
import Codec.Container.Ogg.Track

------------------------------------------------------------
-- Version
--

hoggVersion :: String
hoggVersion = "0.3.0"

showVersion :: IO ()
showVersion = do
    putStrLn $ "hogg version " ++ hoggVersion
    exitWith ExitSuccess

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
    subSynopsis :: String,
    subExamples :: [(String,String)], -- [(example description, flags)]
    subOptions :: [[OptDescr Option]] -- eg. [rangeOptions, cTypeOptions]
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
               sortPagesSub,
               addSkelSub,
               countPacketsSub,
               countrwPagesSub,
               countPagesSub,
               knownCodecsSub,
               selfCheckSub,
               helpSub
              ]

------------------------------------------------------------
-- Options processing
--

data Config =
  Config {
    contentTypeCfg :: Maybe ContentType,
    noSkelCfg :: Bool,
    outputCfg :: Maybe String,
    startCfg :: Maybe Timestamp,
    endCfg :: Maybe Timestamp,
    files :: [FilePath]
  }

dftConfig :: Config
dftConfig =
  Config {
    contentTypeCfg = Nothing,
    noSkelCfg = False,
    outputCfg = Nothing,
    startCfg = Nothing,
    endCfg = Nothing,
    files = ["-"]
  }

-- We will be comparing preset lists of these explicitly, so define an Eq
-- for options. We just compare short options for simplicity.
instance Eq (OptDescr Option) where
  (==) (Option a _ _ _) (Option b _ _ _) =  a == b

-- Options available for subcommands
--
data Option = Help
            | Version
            | ContentTypeOpt String
            | SkelOpt
            | StartOpt String
            | EndOpt String
            | OutputOpt String
            deriving Eq

options :: [OptDescr Option]
options = concat $ miscOptions : allOptions

allOptions :: [[OptDescr Option]]
allOptions = [cTypeOptions, skelOptions, rangeOptions, outputOptions]

miscOptions, cTypeOptions, skelOptions, rangeOptions, outputOptions :: [OptDescr Option]

miscOptions = [
  Option ['h', '?'] ["help"] (NoArg Help) "Display this help and exit",
  Option ['V'] ["version"] (NoArg Version)
         "Output version information and exit" ]

cTypeOptions = [
  Option ['c']      ["content-type"] (ReqArg ContentTypeOpt "Content-Type")
         "Select the logical bitstreams for a specified content type" ]

skelOptions = [
  Option ['k']      ["no-skeleton"] (NoArg SkelOpt)
         "Do NOT include a Skeleton bitstream in the output" ]

rangeOptions = [
  Option ['s']      ["start"] (ReqArg StartOpt "Timestamp")
         "Specify a start time",
  Option ['e']      ["end"] (ReqArg EndOpt "Timestamp")
           "Specify an end time" ]

outputOptions = [
  Option ['o']      ["output"] (ReqArg OutputOpt "filename")
         "Specify output filename" ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
  case getOpt RequireOrder options args of
    (opts, args'  , []  ) -> do
                        processHelp opts
                        config <- processConfig dftConfig opts
                        return (config, args')
    (_, _, _ : _) -> return (dftConfig, args)

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
    processOneOption config (SkelOpt) = do
      return $ config {noSkelCfg = True}
    processOneOption config (OutputOpt output) = do
      return $ config {outputCfg = Just output}
    processOneOption config (StartOpt start) = do
      let s = catchRead "Invalid start time" start
      return $ config {startCfg = Just s}
    processOneOption config (EndOpt end) = do
      let e = catchRead "Invalid end time" end
      return $ config {endCfg = Just e}
    processOneOption config _ = return config

catchRead :: (Read a) => String -> String -> a
catchRead msg x = case reads x of
    [(ms,_mt)] -> ms
    _ -> error (msg ++ ": " ++ x) -- actually [] on error, but catch _ anyway

------------------------------------------------------------
-- Hot: actions for getting Ogg data from the input files
--

-- get the named contents as a bytestring
open :: String -> IO L.ByteString
open f =
#ifdef USE_HTTP
  case (isPrefixOf "http://" f) of
    True -> do
      rsp <- UA.get f
      return $ rspBody rsp
    False ->
#endif
             do
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
    let a = map (mapM f) c
    sequence a

-- | Filter all elements of a chain by the given criteria
chainFilter :: OggChain -> Hot OggChain
chainFilter (OggChain ts gs ps) = do
    ts' <- mType ts ts
    gs' <- mType ts gs
    ps' <- mType ts ps
    return $ OggChain ts' gs' ps'

-- | A generic function to pull a list of things from a chain
chainMatch :: (OggChain -> a) -> Hot [[a]]
chainMatch f = do
    c <- chains
    return $ sequence $ map (map f) c

-- | Filter a ContentTyped list by the given content type
mType :: (ContentTypeImplied a) => [OggTrack] -> [a] -> Hot [a]
mType tks xs = do
    config <- asks hotConfig
    return $ case (contentTypeCfg config) of
      Nothing -> xs
      Just t -> case (noSkelCfg config) of
                  False -> filter (contentTypeImplies tks t) xs
                  True  -> filter (contentTypeIs t) xs

-- | Apply matchRange to all the inner inner lists
matchRange :: (Timestampable a) => [[[a]]] -> Hot [[[a]]]
matchRange as = sequence $ (map (mapM mRange)) as

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
    [("Describe all bitstreams in file.ogg", "file.ogg"),
     ("Describe only the Theora bitstream in file.ogv", "-c theora file.ogv")]
    [cTypeOptions]

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
    [("Dump all bitstreams in file.ogg", "file.ogg"),
     ("Dump only the Theora bitstream in file.ogv", "-c theora file.ogv")]
    allOptions

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
    [("Count packets of all bitstreams in file.ogg", "file.ogg"),
     ("Count packets from only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    [cTypeOptions, rangeOptions]

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
    [("Extract all bitstreams from file.ogg", "-o output.ogg file.ogg"),
     ("Extract only the Theora bitstream from file.ogv",
      "-c theora -o output.ogv file.ogv")]
    allOptions

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
    [("Reconstruct all bitstreams from file.ogg", "-o output.ogg file.ogg"),
     ("Reconstruct only the Theora bitstream from file.ogv",
      "-c theora -o output.ogv file.ogv")]
    allOptions

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
    [("Extract the first minute of file.ogx", "-e 1:00 file.ogx"),
     ("Extract from the second to the fifth minute of file.ogx",
      "-s 2:00 -e 5:00 -o output.ogx file.ogx"),
     ("Extract only the Theora video stream, from 02:00 to 05:00, of file.ogv",
      "-c theora -s 2:00 -e 5:00 -o output.ogv file.ogv"),
     ("Extract, specifying SMPTE-25 frame offsets",
      "-c theora -s smpte-25:00:02:03::12 -e smpte-25:00:05:02::04 -o output.ogv file.ogv")]
    allOptions

chopPages :: Hot ()
chopPages = do
    config <- asks hotConfig
    matchChains <- chains
    chopChains <- mapM (mapM (chopRange config)) matchChains
    let c = \x -> L.concat $ map pageWrite (chainPages x)
    let c2 = \x -> outputPerChain $ map c x
    outputPerFile $ map c2 chopChains

chopRange :: Config -> OggChain -> Hot OggChain
chopRange (Config _ noSkel _ start end _) xs = case noSkel of
  False -> liftIO $ chopWithSkel start end xs
  True  -> liftIO $ chop start end xs

------------------------------------------------------------
-- addSkel (addskel)
--

addSkelSub :: SubCommand
addSkelSub = SubCommand "addskel" addSkel
  "Editing" "Write a Skeleton logical bitstream"
  [("Add a Skeleton to file.ogg", "-o output.oga file.ogg")]
  [outputOptions]

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
    [("Rewrite and count packets of all bitstreams in file.ogg", "file.ogg"),
     ("Rewrite and count packets from only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    [cTypeOptions, rangeOptions]

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
    [("Count pages of all bitstreams in file.ogg", "file.ogg"),
     ("Count pages from only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    [cTypeOptions, rangeOptions]

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
    [("Dump pages of all bitstreams in file.ogg", "file.ogg"),
     ("Dump pages of only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    allOptions

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
    [("Merge pages of audio.oga and video.ogv",
      "-o output.ogv audio.oga video.ogv")]
    [outputOptions]

mergePages :: Hot ()
mergePages = do
    matchPages <- pages
    -- XXX: only use the first chain of each input file. Using subsequent
    -- chains won't work anyway unless corresponding chains in each file are
    -- of identical duration.
    let firstChainPages = map head matchPages
    outputL $ L.concat $ map pageWrite $ merge firstChainPages

------------------------------------------------------------
-- sortPages (sort)
--

sortPagesSub :: SubCommand
sortPagesSub = SubCommand "sort" sortPages
    "Editing" "Rewrite with correct page ordering"
    [("Correct the page ordering in broken.ogv", "-o fixed.ogv broken.ogv")]
    [outputOptions]

sortPages :: Hot ()
sortPages = do
    matchPages <- pages
    let r = \x -> L.concat $ map pageWrite $ sort x
    let r2 = \x -> outputPerChain $ map r x
    outputPerFile $ map r2 matchPages

------------------------------------------------------------
-- dumpRawPages (dumpraw)
--

dumpRawPagesSub :: SubCommand
dumpRawPagesSub = SubCommand "dumpraw" dumpRawPages
    "Reporting" "Dump raw (unparsed) page data"
    [("Dump raw pages of all bitstreams in file.ogg", "file.ogg"),
     ("Dump raw pages of only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    allOptions

dumpRawPages :: Hot ()
dumpRawPages = do
    matchPages <- rawpages
    reportPerFile $ map (C.pack . show) matchPages

------------------------------------------------------------
-- known-types
--

knownCodecsSub :: SubCommand
knownCodecsSub = SubCommand "known-codecs" knownCodecs
  "Miscellaneous" "List codecs known by this version of hogg"
  [] -- Examples
  [] -- Options

knownCodecs :: Hot ()
knownCodecs = liftIO $ mapM_ putStrLn knownContentTypes

------------------------------------------------------------
-- selfcheck
--

selfCheckSub :: SubCommand
selfCheckSub = SubCommand "selfcheck" selfCheck
  "Testing" "Check consistency of help example options"
  [] -- Examples
  [] -- Options

selfCheck :: Hot ()
selfCheck = liftIO $ mapM_ checkArgs allExamples
  where allExamples = concatMap cmd subCommands
        cmd s = map (\e -> unwords ["hogg", subName s, snd e]) (subExamples s)

checkArgs :: String -> IO ()
checkArgs line = do
  let (_:_:args) = words line
  case getOpt RequireOrder options args of
    (_, args'  , []  ) -> do
        -- Allow non options which are either names of subcommands (as
        -- examples for "hogg help"), or filenames (ending in .ogg)
        let a = filter (flip notElem (map subName subCommands)) $
                filter (not . validSuffix) args'
        case a of
          [] -> return ()
          _  -> report (unwords ("non option":(map (\x -> '`':x++['\'']) a)))
    (_, _, errs) -> mapM_ report (map (reverse.tail.reverse) errs)
  where
    report msg = hPutStrLn stderr $ "Warning: " ++ msg ++ " in help example:\n  " ++ line
    validSuffix s = or $ map (flip isSuffixOf s) validExtensions
    validExtensions = [".ogg", ".oga", ".ogv", ".ogx", ".spx"]

------------------------------------------------------------
-- help
--

helpSub :: SubCommand
helpSub = SubCommand "help" help
  "Commands" "Display help for a specific subcommand (eg. \"hogg help chop\")"
  [("Display help for the \"hogg chop\" subcommand", "chop")]
  [] -- Options

help :: Hot ()
help = do
    args <- asks hotFilenames
    outputC $ C.concat $ map C.pack $ longHelp args
    selfCheck

longHelp :: [String] -> [String]
-- | "hogg help" with no arguments: Give a list of all subcommands
longHelp [] =
    ["Usage: hogg <subcommand> [options] filename ...\n\n"] ++
    map categoryHelp ["Commands", "Reporting", "Extraction", "Editing", "Miscellaneous"] ++
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
contextHelp :: [Char] -> [SubCommand] -> [String]
contextHelp command [] = longHelp [] ++ contextError
  where contextError = ["\n*** \"" ++ command ++ "\": Unknown command.\n"]
contextHelp command (item:_) = synopsis ++ usage ++ examples ++
    ["\n" ++ optionsHelp item]
  where usage = ["Usage: hogg " ++ command ++ hasOpts command ++ outOpts]
        hasOpts "help" = " <subcommand>"
        hasOpts _ = " [options]"
        outOpts = case elem outputOptions (subOptions item) of
                    True -> " filename ...\n"
                    False -> "\n"
        synopsis = [command ++ ": " ++ subSynopsis item ++ "\n"]
        examples = case (subExamples item) of
                     [] -> []
                     _  -> ["\nExamples:"] ++
                           flip map (subExamples item) (\(desc,opts) ->
                             "\n  " ++ desc ++ ":\n    hogg " ++ command ++
                             " " ++ opts ++ "\n")

-- | Provide usage information [for a specific command]
optionsHelp :: SubCommand -> String
optionsHelp item = usageInfo "Options:"
                     (concat $ miscOptions : subOptions item)

------------------------------------------------------------
-- main
--

helpStrings :: [[Char]]
helpStrings = ["--help", "-h", "-?"]

versionStrings :: [[Char]]
versionStrings = ["--version", "-V"]

isHelp :: String -> Bool
isHelp x = elem x helpStrings

isVersion :: String -> Bool
isVersion x = elem x versionStrings

initTool :: [String] -> IO HOggTool
initTool args = do
    (config, filenames) <- processArgs args
    return $ HOggTool config filenames

main :: IO ()
main = do
    allArgs <- getArgs
    when (any isHelp allArgs) $ showHelp allArgs
    when (any isVersion allArgs) $ showVersion
    handleSubCommand allArgs

showHelp :: [String] -> IO ()
showHelp allArgs = -- bracket init1 finish1 loop1
  init1 >>= loop1
  where
    init1 = initTool $ filter (not . isHelp) allArgs
    loop1 st = runReaderT run1 st
    run1 = help
    -- finish1 = exitWith ExitSuccess

handleSubCommand :: [String] -> IO ()
handleSubCommand [] = -- bracket (initTool []) finish loop0
  (initTool []) >>= loop0
  where
    -- finish = exitWith ExitSuccess
    loop0 st = runReaderT help st
    
handleSubCommand (command:args) = -- bracket (initTool args) finish loop1
  (initTool args) >>= loop1
  where
    -- finish = exitWith ExitSuccess
    loop1 st = runReaderT run st
    run = act $ filter (\x -> subName x == command) subCommands
    act [] = help
    act (s:_) = (subMethod s)
