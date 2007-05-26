--
-- Module      : Chop
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Chop (
  chop
) where

import Control.Monad.Identity
import Control.Monad.State

import Data.Maybe

import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Timestamp
import Codec.Container.Ogg.Track

------------------------------------------------------------
-- Types
--

data ChopState =
  ChopState {
    headersRemaining :: Int
  }

type Chop a = (StateT ChopState Identity) a

-- | chop start end pages
chop :: Maybe Timestamp -> Maybe Timestamp -> [OggPage] -> [OggPage]
chop start end xs = fst $ runChop emptyChopState (chop1 start end xs)

emptyChopState :: ChopState
emptyChopState = ChopState (fromInteger 0)

runChop :: ChopState -> Chop a -> (a, ChopState)
runChop st x = runIdentity (runStateT x st)

chop1 :: Maybe Timestamp -> Maybe Timestamp -> [OggPage] -> Chop [OggPage]
chop1 Nothing Nothing gs = return gs
chop1 Nothing mEnd@(Just end) gs = return $ takeWhile (before mEnd) gs
chop1 (Just start) mEnd (g:gs) = case (pageBOS g) of
  True -> do
    addHeaders g
    subHeaders g
    cs <- chop1 (Just start) mEnd gs
    return $ g : cs
  False -> do
    r <- gets headersRemaining
    case (compare 0 r) of
      LT -> do
        subHeaders g
        cs <- chop1 (Just start) mEnd gs
        return $ g : cs
      _  -> case (timestampOf g) of
        Nothing -> do
          return g >> (chop1 (Just start) mEnd gs)
        (Just gTime) -> case (compare start gTime) of
          LT -> chop1 Nothing mEnd (g:gs)
          _  -> chop1 (Just start) mEnd gs

-- | Add the total number of headers that this track expects
addHeaders :: OggPage -> Chop ()
addHeaders g = do
  let h = headers $ fromJust (trackType (pageTrack g))
  modifyHeaders h

-- | Subtract the number of completed header packets provided by this page
subHeaders :: OggPage -> Chop ()
subHeaders g = do
  let segs = length $ pageSegments g
      incmplt = pageIncomplete g
      n = if incmplt then (segs-1) else segs
  modifyHeaders (-n)

-- | State modifier to change the number of headers remaining
modifyHeaders :: Int -> Chop ()
modifyHeaders n = do
  r <- gets headersRemaining
  s <- get
  put s{headersRemaining = r + n}

------------------------------------------------------------
-- chop
--

{-
chop :: Maybe Timestamp -> Maybe Timestamp -> [OggPage] -> [OggPage]
chop Nothing Nothing gs = gs
chop Nothing mEnd@(Just end) gs = takeWhile (before mEnd) gs
chop (Just start) mEnd (g:gs) = case (timestampOf g) of
  Nothing -> g : (chop (Just start) mEnd gs)
  (Just gTime) -> case (compare start gTime) of
    LT -> chop Nothing mEnd (g:gs)
    _ -> chop (Just start) mEnd gs
-}

{-
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Word (Word32)

import System.Random

import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Track
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.Skeleton

-- Make a special instance of Random for Word32 that does not include
-- 0xffffffff, as this value is treated specailly by libogg
instance Random Word32 where
  randomR = integralRandomR
  random = randomR (0,0xffffffff-1)

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g') -> (fromIntegral x, g')

-- | Add a Skeleton logical bitstream to an OggChain
chainAddSkeleton :: OggChain -> IO OggChain
chainAddSkeleton chain = do
  serialno <- getStdRandom random
  return $ chainAddSkeleton' serialno chain

-- | Add a Skeleton logical bitstream with a given serialno to an OggChain
chainAddSkeleton' :: Word32 -> OggChain -> OggChain
chainAddSkeleton' serialno (OggChain tracks _ packets) = OggChain nt ng np
  where
    nt = [skelTrack] ++ tracks
    ng = packetsToPages np
    np = [fh] ++ ixBoss ++ ixFisbones ++ ixHdrs ++ [sEOS] ++ ixD

    -- Construct a new track for the Skeleton
    skelTrack = (newTrack serialno){trackType = Just skeleton}

    -- Create the fishead and fisbone packets (all with pageIx 0)
    fh = fisheadToPacket skelTrack emptyFishead
    fbs = map (fisboneToPacket skelTrack) $ tracksToFisbones tracks

    -- Separate out the BOS pages of the input
    (boss, rest) = span packetBOS packets

    -- Increment the pageIx of these original BOS pages by 1, as the
    -- Skeleton fishead packet is being prepended
    ixBoss = map (incPageIx 1) boss

    -- Split the remainder of the input into headers and data
    (hdrs, d) = splitAt totHeaders rest

    -- ... for which we determine the total number of header pages
    totHeaders = foldl (+) 0 tracksNHeaders
    tracksNHeaders = map headers $ mapMaybe trackType tracks

    -- Increment the pageIx of the original data packets by the number of
    -- Skeleton pages
    ixHdrs = map (incPageIx (1 + length fbs)) hdrs
    ixD = map (incPageIx (2 + length fbs)) d

    -- Set the pageIx of the fisbones in turn, beginning after the last
    -- BOS page
    ixFisbones = zipWith setPageIx [1+(length tracks)..] fbs

    -- Generate an EOS packet for the Skeleton track
    sEOS = (uncutPacket L.empty skelTrack sEOSgp){packetEOS = True}
    sEOSgp = Granulepos (Just 0)

-- An internal function for setting the pageIx of the segment of a packet.
-- This is only designed for working with packets which are known to only
-- and entirely span one page, such as Skeleton fisbones.
setPageIx :: Int -> OggPacket -> OggPacket
setPageIx ix p@(OggPacket _ _ _ _ _ (Just [oldSegment])) =
  p{packetSegments = Just [newSegment]}
  where
    newSegment = oldSegment{segmentPageIx = ix}
setPageIx _ _ = error "setPageIx used on non-uncut page"

-- An internal function for incrementing the pageIx of all the segments of
-- a packet.
incPageIx :: Int -> OggPacket -> OggPacket
incPageIx ixd p@(OggPacket _ _ _ _ _ (Just segments)) =
  p{packetSegments = Just (map incSegIx segments)}
  where
    incSegIx :: OggSegment -> OggSegment
    incSegIx s@(OggSegment _ oix _) = s{segmentPageIx = oix + ixd}
-- Otherwise, the packet has no segmentation info so leave it untouched
incPageIx _ p = p

-}
