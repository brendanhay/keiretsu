{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}

-- Module      : Keiretsu.Process
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Keiretsu.Process
    ( run
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception         (SomeException, bracket, catch, finally)
import           Control.Monad
import           Data.ByteString           (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Conduit.List         as Conduit
import qualified Data.Conduit.Network.Unix as Conduit
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import           Keiretsu.Log
import           Keiretsu.Orphans          ()
import           Keiretsu.Types
import           Network.Socket
import           System.Console.ANSI
import           System.Directory          (removeFile)
import           System.Exit
import           System.IO
import           System.Process

-- FIXME: Check syslog sockets should be GCd

data Cmd = Cmd
    { cmdPrefix :: Text
    , cmdString :: Text
    , cmdHd     :: ProcessHandle
    } deriving (Eq, Show)

run :: [Proc] -> IO ()
run [] = return ()
run ps = bracket openSyslog closeSyslog $ \sock -> do
    pids   <- foldM (collect sock) [] (zip colours ps)
    (p, c) <- snd <$> (waitProcesses pids >>= waitAny)
    exit (filter (/= p) pids) (p, c)
  where
    collect sock pids (col, p) =
        process sock col out p >>=
            either (exit pids) (return . (: pids))

    out = Conduit.sinkHandle stdout

    exit pids (Cmd{..}, c) = do
        terminate pids
        logDebug $ "Exiting with "
            ++ show c
            ++ ":\n  "
            ++ Text.unpack (cmdPrefix <> ": " <> cmdString)
        exitWith c

process :: Socket
        -> Color
        -> Consumer ByteString IO ()
        -> Proc
        -> IO (Either (Cmd, ExitCode) Cmd)
process sock col out Proc{..} = do
    hd <- connectToSyslog sock col out procPrefix
    p  <- create hd procCmd
    case procCheck of
        Nothing  -> delay >> return (Right p)
        Just chk -> check p chk procRetry
  where
    check o chk n = do
        hd <- connectToSyslog sock col out (procPrefix <> "/check")

        hPutStrLn hd ("Delaying for " ++ show procDelay ++ "ms ...")
        delay

        p <- create hd chk
        c <- waitForProcess (cmdHd p)

        if | c == ExitSuccess -> return (Right o)
           | n <= 0           -> return (Left (p, c)) `finally` terminate [o]
           | otherwise        -> check o chk (n - 1)

    create hd cmd = do
        Text.hPutStrLn hd cmd
        (_, _, _, p) <- createProcess $ processSettings hd cmd
        return $! Cmd procPrefix cmd p

    processSettings hd cmd = (shell $ Text.unpack cmd)
        { std_out = UseHandle hd
        , std_err = UseHandle hd
        , close_fds = False
        , env     = if null env then Nothing else Just env
        , cwd     = Just (depPath procDep)
        }

    env = map (Text.unpack *** Text.unpack) procEnv

    delay = threadDelay (procDelay * 1000)

terminate :: [Cmd] -> IO ()
terminate = mapM_ $ \Cmd{..} -> terminateProcess cmdHd <* waitForProcess cmdHd

waitProcesses :: [Cmd] -> IO [Async (Cmd, ExitCode)]
waitProcesses cs = forM cs $ \c -> async $ (c,) <$> waitForProcess (cmdHd c)

syslogSock :: String
syslogSock = "keiretsu.syslog"

openSyslog :: IO Socket
openSyslog = open `catch` printErr
  where
    open = do
        l <- socket AF_UNIX Stream defaultProtocol
        bind l (SockAddrUnix syslogSock)
        listen l 128
        return l

    printErr :: SomeException -> a
    printErr _ = error $ "Unable to open syslog socket '" ++ syslogSock ++ "'"

closeSyslog :: Socket -> IO ()
closeSyslog s = close s `finally` removeFile syslogSock

connectToSyslog :: Socket
                -> Color
                -> Consumer ByteString IO ()
                -> Text
                -> IO Handle
connectToSyslog sock col out p = do
    lock <- newEmptyMVar
    a    <- async $ prepare lock

    link a

    cl <- socket AF_UNIX Stream defaultProtocol
    connect cl (SockAddrUnix syslogSock)

    hd <- socketToHandle cl WriteMode
    hSetBuffering hd LineBuffering

    takeMVar lock

    return hd
  where
    prepare lock = do
        s <- fst <$> accept sock <* putMVar lock ()
        Conduit.sourceSocket s
            $= Conduit.lines
            $= Conduit.map (colourise col p)
            $$ Conduit.map (<> "\n")
            =$ out
