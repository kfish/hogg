--
-- Module      : Granulepos
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Ogg.Granulepos (
  Granulepos (..)
) where

import Data.Word (Word64)

------------------------------------------------------------
-- Types
--

newtype Granulepos = Granulepos (Maybe Word64)

------------------------------------------------------------
-- Granulepos functions
--

instance Show Granulepos where
  show (Granulepos (Nothing)) = "-1"
  show (Granulepos (Just gp)) = show gp
