module Main where

import Control.Monad

import System.Environment (getArgs, getProgName)
import System.IO

import System.Console.GetOpt
import System.Exit

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Ogg.Chain
import Ogg.RawPage
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

-- Options available for subcommands
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

mTracks :: [String] -> IO [OggTrack]
mTracks args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allTracks <- getTracks filename
    return $ trackMatch ctype allTracks

mRawPages :: [String] -> IO [OggRawPage]
mRawPages args = do
    (config, filenames) <- processArgs args
    -- let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allRawPages <- getRawPages filename
    return allRawPages

mPages :: [String] -> IO [OggPage]
mPages args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPages <- getPages filename
    return $ pageMatch ctype allPages

mPackets :: [String] -> IO [OggPacket]
mPackets args = do
    (config, filenames) <- processArgs args
    let ctype = parseType $ contentTypeCfg config
    let filename = head filenames
    allPackets <- {-# SCC "getPackets" #-}getPackets filename
    return $ packetMatch ctype allPackets

info :: [String] -> IO ()
info args = do
    matchTracks <- mTracks args
    C.putStr $ C.concat $ map (C.pack . show) matchTracks

dumpPackets :: [String] -> IO ()
dumpPackets args = do
    matchPackets <- {-# SCC "matchPackets" #-}mPackets args
    -- mapM_ putStrLn (map show matchPackets)
    -- C.putStrLn $ C.concat $ map ({-# SCC "Cpack" #-}C.pack . show) matchPackets
    C.putStrLn $ C.concat $ map packetToBS matchPackets

countPackets :: [String] -> IO ()
countPackets args = do
    matchPackets <- mPackets args
    putStrLn $ show (length matchPackets) ++ " packets"

rewritePages :: [String] -> IO ()
rewritePages args = do
    matchPages <- mPages args
    -- mapM_ L.putStr (map pageWrite matchPages)
    L.putStr $ L.concat (map pageWrite matchPages)

rewritePackets :: [String] -> IO ()
rewritePackets args = do
    matchPackets <- mPackets args
    -- mapM_ L.putStr (map pageWrite (packetsToPages matchPackets))
    L.putStr $ L.concat (map pageWrite (packetsToPages matchPackets))

countrwPages :: [String] -> IO ()
countrwPages args = do
    matchPages <- mPages args
    putStrLn $ show $ length (packetsToPages (pagesToPackets matchPages))

countPages :: [String] -> IO ()
countPages args = do
    matchPages <- mPages args
    putStrLn $ (show $ length matchPages) ++ " pages"

dumpPages :: [String] -> IO ()
dumpPages args = do
  matchPages <- mPages args
  C.putStrLn $ C.concat $ map (C.pack . show) matchPages

dumpRawPages :: [String] -> IO ()
dumpRawPages args = do
  matchPages <- mRawPages args
  -- mapM_ putStrLn (map show matchPages)
  C.putStrLn $ C.concat $ map (C.pack . show) matchPages

getFilename :: [String] -> IO String
getFilename args = return $ last args

helpCommand :: String -> String -> IO ()
helpCommand command desc = do
  putStrLn $ "\t" ++ command ++ "\t\t" ++ desc

helpCommands :: IO ()
helpCommands = do
  putStrLn "hogg"
  helpCommand "info" "Print info about tracks"
  helpCommand "dump" "Dump packets"
  helpCommand "pagecount" "Count pages" 
  helpCommand "rewrite" "Rewrite the file via pages"
  helpCommand "repacket" "Rewrite the file via packets"
  helpCommand "countrw" "Rewrite and count"
  helpCommand "dumpraw" "Dump raw pages"

main :: IO ()
main = do
    (command:args) <- getArgs
    filename <- getFilename args
    case command of
      "help" -> helpCommands
      "info" -> info args
      "dump" -> dumpPackets args
      "packetcount" -> countPackets args
      "pagecount" -> countPages args
      "pagedump" -> dumpPages args
      "rewrite" -> rewritePages args
      "repacket" -> rewritePackets args
      "countrw" -> countrwPages args
      "dumpraw" -> dumpRawPages args
      _ -> helpCommands
