#!/usr/bin/env runhaskell

import Distribution.Simple (defaultMainWithHooks, defaultUserHooks,
                             UserHooks(..), Args)
import           Distribution.PackageDescription (PackageDescription)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import           Distribution.Simple.Utils (rawSystemVerbose)
import           Distribution.Compat.FilePath (joinPaths)
import System.Exit(ExitCode(..))

main :: IO ()
main = defaultMainWithHooks (defaultUserHooks{runTests = tests})

-- The following two lines were for older cabal version
-- tests :: Args -> Bool -> LocalBuildInfo -> IO ExitCode
-- tests args _ lbi =

tests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ExitCode
tests args _ _ lbi =
   let testCmd = foldl1 joinPaths [LBI.buildDir lbi, "ListMergeTest", "ListMergeTest"]
   in  rawSystemVerbose 1 testCmd
          ("+RTS" : "-M32m" : "-c30" : "-RTS" : args)
