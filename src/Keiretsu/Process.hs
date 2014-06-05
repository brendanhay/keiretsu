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
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as Text
import           Data.Traversable          (sequenceA)
import           Keiretsu.Log
import           Keiretsu.Orphans          ()
import           Keiretsu.Types
import           Network.Socket
import           System.Console.ANSI
import           System.Directory          (removeFile)
import           System.Exit
import           System.IO
import           System.Process

run :: [Proc] -> IO ()
run [] = return ()
run ps = bracket openSyslog closeSyslog $ \slog -> do
    let out = Conduit.sinkHandle stdout

    rs <- forM (zip colours ps) $ \(c, p) ->
        prepareSyslog slog c out p >> process p

    let (pids, chks) = unzip rs

    waitAsyncProcess (catMaybes chks)
        >>= mapM wait
        >>= check

    (_, (p, c)) <- waitAsyncProcess pids >>= waitAny

    terminate (filter (/= p) pids)

    logDebug $ "Exiting with " ++ show c
    exitWith c

check :: [(ProcessHandle, ExitCode)] -> IO ()
check [] = return ()
check ((_, c) : xs)
    | c == ExitSuccess = check xs
    | otherwise        = terminate (map fst xs)

process :: Proc -> IO (ProcessHandle, Maybe ProcessHandle)
process Proc{..} = do
    hd <- connectToSyslog
    (,) <$> (create hd procCmd <* threadDelay procDelay)
        <*> sequenceA (create hd <$> procCheck)
  where
    create hd cmd = do
        logDebug $ "Running " ++ show hd ++ " command: " ++ Text.unpack cmd
        (_, _, _, p) <- createProcess $ processSettings hd cmd
        return p

    processSettings hd cmd = (shell $ Text.unpack cmd)
        { std_out = UseHandle hd
        , std_err = UseHandle hd
        , env     = if null env then Nothing else Just env
        , cwd     = Just (depPath procDep)
        }

    env = map (Text.unpack *** Text.unpack) procEnv

terminate :: [ProcessHandle] -> IO ()
terminate = mapM_ terminateProcess

waitAsyncProcess :: [ProcessHandle] -> IO [Async (ProcessHandle, ExitCode)]
waitAsyncProcess ps = forM ps $ \p -> async $ (p, ) <$> waitForProcess p

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
