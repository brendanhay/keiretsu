{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main
    ( main
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Keiretsu.Config
import           Keiretsu.Process
import           Keiretsu.Types
import           Options
import           System.Console.ANSI
import           System.Directory
import           System.Environment

defineOptions "Start" $ do
    stringOption "sDir" "dir" "./"
        "Path to the directory containing the root Intfile."

    boolOption "sDebug" "debug" False
        "Show debug output."

    stringsOption "sEnvs" "env" []
        "Additional .env files to merge into the environment."

    stringsOption "sRuns" "run" []
        "Additional commands to run in the environment."

    intOption "sDelay" "delay" 1
        "Delay after dependency start, before forking --run arguments."

    stringsOption "sExclude" "exclude" []
        "Proctypes to exclude."

    boolOption "sDryRun" "dry-run" False
        "Read, parse and print output without starting any processes."

    boolOption "sColor" "Color" True
        "Colourise output."

    stringOption "sShell" "shell" "/bin/sh"
        "Shell to run commands under."

main :: IO ()
main = runCommand $ \opts@Start{..} _ -> runScript $ do
    check opts
    scriptIO $ do
        d  <- makeLocalDep
        ds <- reverse . (d :) <$> loadDeps sDir
        ps <- readProcs ds
        pe <- readEnvs ds sEnvs ps
        le <- getEnvironment
        ex <- mapM (makeLocalProc "run") sRuns

        let disc = makeCmds pe 0 ps
            spec = makeCmds (pe ++ le) sDelay ex
            cmds = filter ((`notElem` sExclude) . cmdPre) $ disc ++ spec

        when sDebug $ dumpEnv cmds
        when (not sDryRun) $ runCommands cmds

check :: Start -> Script ()
check Start{..} = do
    when (0 > sDelay) $ throwT "--delay must be non-negative."
    when (null sDir) $ throwT "--dir must be specified."
    mapM_ (path " specified by --env doesn't exist.") sEnvs
  where
    path m f = do
        p <- scriptIO $ doesFileExist f
        when (not p) . throwT $ f ++ m

dumpEnv :: [Cmd] -> IO ()
dumpEnv = mapM_ (mapM_ BS.putStrLn) . map formatEnv . zip colours

formatEnv :: (Color, Cmd) -> [ByteString]
formatEnv (c, Cmd{..}) = map (colourise c "") $
    BS.pack cmdPre <> ": " <> BS.pack cmdStr : map f cmdEnv
  where
    f (k, v) = BS.pack k <> ": " <> BS.pack v

