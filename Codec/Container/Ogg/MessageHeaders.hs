--
-- Module      : MessageHeaders
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.MessageHeaders (
  MessageHeaders(..),
  mhEmpty,
  mhSingleton,
  mhInsert,
  mhAppend
) where

import Data.List as List
import Data.Map as Map

------------------------------------------------------------
-- Data
--

data MessageHeaders =
  MessageHeaders {
    mhHeaders :: Map.Map String [String]
  }
    
------------------------------------------------------------
-- Constructors
--

mhEmpty :: MessageHeaders
mhEmpty = MessageHeaders (Map.empty)

mhSingleton :: String -> String -> MessageHeaders
mhSingleton f v = MessageHeaders (Map.singleton f [v])

------------------------------------------------------------
-- Insert
--

mhInsert :: String -> String -> MessageHeaders -> MessageHeaders
mhInsert k v (MessageHeaders h) = MessageHeaders (Map.insert k [v] h)

------------------------------------------------------------
-- Append
--

mhAppend :: String -> String -> MessageHeaders -> MessageHeaders
mhAppend k v (MessageHeaders h) = MessageHeaders (Map.insertWith (++) k [v] h)

------------------------------------------------------------
-- Show
--

instance Show MessageHeaders where
  show (MessageHeaders h) =
    concat $ List.map serializeMH (assocs h)
    where
      serializeMH :: (String, [String]) -> String
      serializeMH (k, v) = k ++ ": " ++ (concat $ intersperse ", " v) ++ "\r\n"
