{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    ( runCommands
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception        (SomeException, bracket, catch, finally)
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.Monoid
import           Keiretsu.Log
import           Keiretsu.Types
import           Network.Socket
import           System.Console.ANSI
import           System.Directory         (removeFile)
import           System.Exit
import           System.IO
import           System.IO.Streams        (OutputStream)
import qualified System.IO.Streams        as Streams
import           System.Posix.Process     ()
import           System.Process
import           System.Process.Internals

instance Eq ProcessHandle where
    (ProcessHandle a _) == (ProcessHandle b _) = a == b

runCommands :: [Cmd] -> IO ()
runCommands []   = return ()
runCommands cmds = bracket openSyslog closeSyslog $ \slog -> do
    out  <- Streams.lockingOutputStream Streams.stdout
    pids <- forM (zip colours cmds) $ \(col, exe) -> do
        prepareSyslog slog col out exe
        runCmd exe
    (_, (p, code)) <- waitProcess pids >>= waitAny
    terminate (filter (/= p) pids)
    logDebug $ "Exiting with " ++ show code
    exitWith code

runCmd :: Cmd -> IO ProcessHandle
runCmd Cmd{..} = do
    hd <- connectToSyslog
    (_, _, _, p) <- createProcess $ processSettings hd
    threadDelay cmdDelay
    return p
  where
    processSettings hd = (shell cmdStr)
        { std_out = UseHandle hd
        , std_err = UseHandle hd
        , env     = if null cmdEnv then Nothing else Just cmdEnv
        , cwd     = cmdDir
        }

terminate :: [ProcessHandle] -> IO ()
terminate = mapM_ terminateProcess

waitProcess :: [ProcessHandle] -> IO [Async (ProcessHandle, ExitCode)]
waitProcess ps = forM ps $ \p -> async $ (p, ) <$> waitForProcess p

syslogSock :: String
syslogSock = "keiretsu.syslog"

openSyslog :: IO Socket
openSyslog = open `catch` printErr
  where
    open = do
        logger <- socket AF_UNIX Stream defaultProtocol
        bind logger (SockAddrUnix syslogSock)
        listen logger 128
        return logger

    printErr :: SomeException -> a
    printErr _ = error $ "Unable to open syslog socket '" ++ syslogSock ++ "'"

closeSyslog :: Socket -> IO ()
closeSyslog s = close s `finally` removeFile syslogSock

connectToSyslog :: IO Handle
connectToSyslog = do
    client <- socket AF_UNIX Stream defaultProtocol
    connect client (SockAddrUnix syslogSock)
    handle <- socketToHandle client WriteMode
    hSetBuffering handle LineBuffering
    return handle

prepareSyslog :: Socket -> Color -> OutputStream ByteString -> Cmd -> IO ()
prepareSyslog syslog col out cmd = void . forkIO $ do
    remote <- accept syslog
    (i, _) <- Streams.socketToStreams (fst remote)
    o      <- Streams.unlines out
    Streams.lines i
        >>= Streams.map (colourise col $ BS.pack (cmdPre cmd) <> ": ")
        >>= flip Streams.connect o
