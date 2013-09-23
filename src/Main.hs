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
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Keiretsu.Config
import           Keiretsu.Process
import           Keiretsu.Types
import           Options
import           System.Directory
import           System.Environment

defineOptions "Start" $ do
    stringOption "sDir" "dir" "./"
        "Path to the directory containing the root Intfile. (default: ./)"

    boolOption "sDebug" "debug" False
        "Show debug output. (default: false)"

    stringsOption "sEnvs" "env" []
        "Additional .env files to merge into the environment. (default: none)"

    stringsOption "sRuns" "run" []
        "Additional commands to run in the environment. (default: none)"

    intOption "sDelay" "delay" 1000
        "Millisecond delay between dependency start. (default 1000)"

    stringsOption "sExclude" "exclude" []
        "Name of a proctype to exclude. (default: none)"

    boolOption "sDryRun" "dry-run" False
        "Print output without starting any processes. (default: false)"

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

        let delay = sDelay * 1000
            disc  = makeCmds pe delay ps
            spec  = makeCmds (pe ++ le) delay ex
            cmds  = filter ((`notElem` sExclude) . cmdPre) $ disc ++ spec

        when sDebug $ dump cmds
        unless sDryRun $ runCommands cmds

check :: Start -> Script ()
check Start{..} = do
    when (0 > sDelay) $ throwT "--delay must be non-negative."
    when (null sDir) $ throwT "--dir must be specified."
    mapM_ (path " specified by --env doesn't exist.") sEnvs
  where
    path m f = do
        p <- scriptIO $ doesFileExist f
        unless p . throwT $ f ++ m

dump :: [Cmd] -> IO ()
dump = mapM_ (mapM_ BS.putStrLn . format) . zip colours
  where
    format (c, Cmd{..}) = map (colourise c "") $
        BS.pack cmdPre <> ": " <> BS.pack cmdStr : map f cmdEnv

    f (k, v) = BS.pack k <> ": " <> BS.pack v
