{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List             (nub)
import           Data.Monoid
import           Keiretsu.Config
import           Keiretsu.Log
import           Keiretsu.Process
import           Keiretsu.Types
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Exit

data Start = Start
    { sDir     :: !FilePath
    , sDebug   :: !Bool
    , sEnvs    :: [FilePath]
    , sRuns    :: [String]
    , sDelay   :: !Int
    , sExclude :: [FilePath]
    , sDryRun  :: !Bool
    , sPorts   :: !Int
    }

start :: Parser Start
start = Start
    <$> strOption
        ( long "dir"
       <> short 'd'
       <> metavar "DIR"
       <> value "./"
       <> help "Path to the directory containing the root Intfile. (default: ./)"
        )
    <*> switch
        ( long "debug"
       <> help "Show debug output. (default: false)"
        )
    <*> many (strOption
        ( long "env"
       <> short 'e'
       <> metavar "FILE"
       <> help "Additional .env files to merge into the environment. (default: none)"
        ))
    <*> many (strOption
        ( long "run"
       <> short 'r'
       <> metavar "CMD"
       <> help "Additional commands to run in the environment. (default: none)"
        ))
    <*> option
        ( long "delay"
       <> short 'n'
       <> metavar "MS"
       <> value 1000
       <> help "Millisecond delay between dependency start. (default: 1000)"
        )
    <*> many (strOption
        ( long "exclude"
       <> short 'x'
       <> metavar "PROC"
       <> help "Name of a proctype to exclude. (default: none)"
        ))
    <*> switch
        ( long "dry-run"
       <> help "Print output without starting any processes. (default: false)"
        )
    <*> option
        ( long "ports"
       <> short 'p'
       <> metavar "INT"
       <> value 2
       <> help "Number of ports to allocate to a single proctype. (default: 2)"
        )

main :: IO ()
main = do
    s@Start{..} <- customExecParser
        (prefs $ showHelpOnError <> columns 100)
        (info start idm)

    setLogging sDebug
    check s

    d  <- makeLocalDep
    ds <- reverse . nub . (d :) <$> loadDeps sDir
    ps <- readProcs sPorts ds
    pe <- readEnvs ds sEnvs ps
    le <- getEnvironment
    pr <- mapM (makeLocalProc "run") sRuns

    let delay = sDelay * 1000
        disc  = makeCmds pe delay ps
        spec  = makeCmds (pe ++ le) delay pr
        cmds  = filter ((`notElem` sExclude) . cmdPre) $ disc ++ spec

    print disc

    when sDebug $ dumpEnv cmds
    unless sDryRun $ runCommands cmds

check :: Start -> IO ()
check Start{..} = do
    unless (sDelay >= 0) $ throwError "--delay must be non-negative."
    unless (sPorts >= 1) $ throwError "--ports must be greater-than 0."
    when (null sDir) $ throwError "--dir must be specified."
    mapM_ (path " specified by --env doesn't exist.") sEnvs
  where
    path m f = do
        p <- doesFileExist f
        unless p . throwError $ f ++ m

    throwError s = logError s >> exitFailure

dumpEnv :: [Cmd] -> IO ()
dumpEnv = mapM_ (mapM_ BS.putStrLn . format) . zip colours
  where
    format (c, Cmd{..}) = map (colourise c "") $
        BS.pack cmdPre <> ": " <> BS.pack cmdStr : map f cmdEnv

    f (k, v) = BS.pack k <> ": " <> BS.pack v
