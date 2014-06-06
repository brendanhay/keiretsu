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
import           Control.Arrow
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
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
    , sEnvs    :: [FilePath]
    , sRuns    :: [Text]
    , sDelay   :: !Int
    , sExclude :: [Text]
    , sPorts   :: !Int
    , sDryRun  :: !Bool
    , sDebug   :: !Bool
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

    <*> many (strOption
        ( long "env"
       <> short 'e'
       <> metavar "FILE"
       <> help "Additional .env files to merge into the environment. (default: none)"
        ))

    <*> many (textOption
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
       <> help "Millisecond delay between run commands start. (default: 1000)"
        )

    <*> many (textOption
        ( long "exclude"
       <> short 'x'
       <> metavar "PROC"
       <> help "Prefixed name of a proctype to exclude. (default: none)"
        ))

    <*> option
        ( long "ports"
       <> short 'p'
       <> metavar "INT"
       <> value 2
       <> help "Number of ports to allocate to a single proctype. (default: 2)"
        )

    <*> switch
        ( long "dry-run"
       <> help "Print output without starting any processes. (default: false)"
        )

    <*> switch
        ( long "debug"
       <> help "Show debug output. (default: false)"
        )

main :: IO ()
main = do
    s@Start{..} <- customExecParser
        (prefs $ showHelpOnError <> columns 100)
        (info start idm)

    setLogging sDebug
    check s

    l  <- depLocal
    ds <- (l :) <$> dependencies sDir
    ps <- excludeProcs sExclude . concat <$> mapM (proctypes sPorts) ds
    pe <- environment ps sEnvs
    le <- (pe ++) . map (Text.pack *** Text.pack) <$> getEnvironment

    let cs    = map (setLocalEnv pe) ps
        rs    = zipWith (procLocal l le sDelay) [1..] sRuns
        procs = cs ++ excludeProcs sExclude rs

    when sDebug $
        dump procs

    unless sDryRun $
        run procs

check :: Start -> IO ()
check Start{..} = do
    when (sPorts < 0) $ throwError "--ports must be greater-than 0."
    when (null sDir)  $ throwError "--dir must be specified."

    d <- doesDirectoryExist sDir
    unless d . throwError $
        "Directory " ++ sDir ++ " specified by --dir doesn't exist."

    forM_ sEnvs $ \e -> do
        f <- doesFileExist e
        unless f . throwError $
            "File " ++ e ++ " specified by --env doesn't exist."

throwError :: String -> IO ()
throwError s = logError s >> exitFailure

dump :: [Proc] -> IO ()
dump = zipWithM_ (\c -> mapM_ BS.putStrLn . format c) colours
  where
    format c Proc{..} = map (colourise c procPrefix . Text.encodeUtf8)
        $ "command: " <> procCmd
        : "delay: "   <> Text.pack (show procDelay ++ "ms")
        : maybe [] (\x -> ["check: " <> x]) procCheck
       ++ map f procEnv

    f (k, v) = k <> ": " <> v

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap Text.pack . strOption
