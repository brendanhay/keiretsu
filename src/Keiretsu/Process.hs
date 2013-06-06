{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Keiretsu.Process (runCommands) where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket, finally)
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid
import Keiretsu.Types
import Network.Socket
import System.Console.ANSI
import System.Directory (removeFile)
import System.Exit
import System.IO
import System.IO.Streams (OutputStream)
import System.Posix.Process ()
import System.Process
import qualified System.IO.Streams as Streams

type Stdout = OutputStream ByteString

runCommands :: [Cmd] -> IO ()
runCommands   [] = return ()
runCommands cmds = bracket openSyslog closeSyslog $ \slog -> do
    out  <- Streams.lockingOutputStream Streams.stdout
    pids <- forM (zip colours cmds) $ \(col, exe) -> do
        prepareSyslog slog col out exe
        runCmd exe

    (_, (p, code)) <- waitProcess pids >>= waitAny

    terminate (filter (/= p) pids)
    exitWith code

runCmd :: Cmd -> IO ProcessHandle
runCmd cmd = do
    handle       <- connectToSyslog
    (_, _, _, p) <- createProcess (processSettings handle cmd)
    threadDelay 300000
    return p
  where
    processSettings handle Cmd {..} =
        (shell cmdStr)
            { std_out = UseHandle handle
            , std_err = UseHandle handle
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
openSyslog = do
    logger <- socket AF_UNIX Stream defaultProtocol
    bind logger (SockAddrUnix syslogSock)
    listen logger 128
    return logger

closeSyslog :: Socket -> IO ()
closeSyslog s = close s `finally` removeFile syslogSock

connectToSyslog :: IO Handle
connectToSyslog = do
    client <- socket AF_UNIX Stream defaultProtocol
    connect client (SockAddrUnix syslogSock)
    handle <- socketToHandle client WriteMode
    hSetBuffering handle LineBuffering
    return handle

prepareSyslog :: Socket -> Color -> Stdout -> Cmd -> IO ()
prepareSyslog syslog col out cmd = void . forkIO $ do
    remote <- accept syslog
    (i, _) <- Streams.socketToStreams (fst remote)
    let prefix = pack (cmdPre cmd) <> ": "
    o <- Streams.unlines out
    Streams.lines i
        >>= Streams.map (colourise col prefix <>)
        >>= flip Streams.connect o
