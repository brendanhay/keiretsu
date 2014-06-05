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
import           Keiretsu.Log
import           Keiretsu.Orphans          ()
import           Keiretsu.Types
import           Network.Socket
import           System.Console.ANSI
import           System.Directory          (removeFile)
import           System.Exit
import           System.IO
import           System.Process

run :: [Dep] -> IO ()
run [] = return ()
run ds = bracket openSyslog closeSyslog $ \slog -> do
    let out = Conduit.sinkHandle stdout

    pids <- fmap concat . forM (zip ds colours) $ \(Dep{..}, c') ->
        forM (zip (dropWhile (/= c') colours) depProcs) $ \(c, p) -> do
            prepareSyslog slog c out p
            process depDelay depCheck depPath p

    (_, (p, c)) <- waitProcess pids >>= waitAny

    terminate (filter (/= p) pids)

    logDebug $ "Exiting with " ++ show c
    exitWith c

process :: Int -> Maybe Text -> FilePath -> Proc -> IO ProcessHandle
process n chk cwd Proc{..} = do
    hd <- connectToSyslog
    (_, _, _, p) <- createProcess $ processSettings hd
    threadDelay n
    return p
  where
    processSettings hd = (shell $ Text.unpack procCmd)
        { std_out = UseHandle hd
        , std_err = UseHandle hd
        , env     = if null env then Nothing else Just env
        , cwd     = Just cwd
        }

    env = map (Text.unpack *** Text.unpack) procEnv

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

prepareSyslog :: Socket -> Color -> Consumer ByteString IO () -> Proc -> IO ()
prepareSyslog syslog col out p = void . forkIO $ do
    sock <- fst <$> accept syslog
    Conduit.sourceSocket sock
        $= Conduit.lines
        $= Conduit.map (colourise col (procPrefix p))
        $$ Conduit.map (<> "\n")
        =$ out
