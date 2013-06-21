{-# LANGUAGE OverloadedStrings #-}

module Keiretsu.Command (
      clean
    , start
    , integrate
    ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString          (ByteString)
import Data.ByteString.Char8    (pack)
import Data.Monoid
import System.Console.ANSI
import System.Environment

import Keiretsu.Config
import Keiretsu.Dependency
import Keiretsu.Process
import Keiretsu.Types

import qualified Data.ByteString.Char8 as S

clean :: FilePath -> FilePath -> Bool -> IO ()
clean cfg tmp force = readIntfile cfg tmp >>= mapM_ (wipe force)

start :: [FilePath] -> Bool -> IO ()
start paths fdump = do
    dep  <- makeLocalDep
    ps   <- readProcfile dep
    env  <- readEnvironments paths ps

    let cmds = makeCmds env 0 ps

    when fdump $ dumpEnv cmds
    runCommands cmds

integrate :: FilePath
          -> FilePath
          -> [FilePath]
          -> [String]
          -> [String]
          -> Int
          -> Bool
          -> Bool
          -> Bool
          -> IO ()
integrate cfg tmp paths runs excls delay fdump fverify fbuild = do
    ex   <- mapM (makeLocalProc "run") runs
    dep  <- makeLocalDep
    ds   <- (dep :) <$> readIntfile cfg tmp

    whenFlag fverify verify ds
    whenFlag fbuild build ds

    ps   <- readProcfiles ds
    penv <- readEnvironments paths ps
    lenv <- getEnvironment

    let disc = makeCmds penv 0 ps
        spec = makeCmds (penv ++ lenv) delay ex
        cmds = filter ((`notElem` excls) . cmdPre) $ disc ++ spec

    when fdump $ dumpEnv cmds
    runCommands cmds

whenFlag :: Bool -> (a -> IO b) -> [a] -> IO ()
whenFlag p f = when p . void . mapConcurrently f

dumpEnv :: [Cmd] -> IO ()
dumpEnv = mapM_ (mapM_ S.putStrLn) . map formatEnv . zip colours

formatEnv :: (Color, Cmd) -> [ByteString]
formatEnv (c, x) = map (colourise c "") $ "Environment: " <> pack (cmdStr x) : map f (cmdEnv x)
  where
    f (k, v) = pack k <> ": " <> pack v
