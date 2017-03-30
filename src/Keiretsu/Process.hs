{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Keiretsu.Process
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C8
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
import           Network.Socket            as Socket
import           System.Console.ANSI
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process            hiding (runProcess)

data Cmd = Cmd
    { cmdPrefix :: Text
    , cmdColour :: !Color
    , cmdString :: Text
    , cmdHd     :: ProcessHandle
    , cmdLog    :: Async ()
    } deriving (Eq)

run :: [Proc] -> IO ()
run [] = return ()
run ps = bracket openSyslog closeSyslog $ \sock -> do
    cs          <- foldM (collect sock) [] (zip colours ps)
    as          <- mapM waitProcessAsync cs
    (a, (p, c)) <- waitAny as
    exit (filter (/= p) cs) (filter (/= a) as) (p, c)
  where
    collect sock cs (col, p) =
        runProcess sock col out p >>=
            either (exit cs []) (return . (: cs))

    out = Conduit.sinkHandle stdout

    exit cs as (Cmd{..}, c) = do
        terminate cs `finally` mapM_ cancel as
        logDebug $ "Exiting with "
            ++ show c
            ++ ":\n  "
            ++ Text.unpack (cmdPrefix <> ": " <> cmdString)
        exitWith c

runProcess :: Socket
           -> Color
           -> Consumer ByteString IO ()
           -> Proc
           -> IO (Either (Cmd, ExitCode) Cmd)
runProcess sock col out Proc{..} = do
    when procEphem delay
    c <- connectToSyslog sock col out procPrefix
    p <- create c procCmd
    case procCheck of
        Nothing | procEphem -> return (Right p)
        Nothing             -> delay >> return (Right p)
        Just chk            -> check p chk procRetry
  where
    check o chk n = do
        (a, hd) <- connectToSyslog sock col out (procPrefix <> "/check")
        hPutStrLn hd ("Delaying for " ++ show procDelay ++ "ms ...")
        delay
        p <- create (a, hd) chk
        c <- waitForProcess (cmdHd p)
        if | c == ExitSuccess -> return (Right o)
           | n <= 0           -> return (Left (p, c)) `finally` terminate [o]
           | otherwise        -> check o chk (n - 1)

    create (a, hd) cmd = do
        Text.hPutStrLn hd cmd
        (_, _, _, p) <- createProcess $ processSettings hd cmd
        return $! Cmd procPrefix col cmd p a

    processSettings hd cmd = (shell $ Text.unpack cmd)
        { std_out       = UseHandle hd
        , std_err       = UseHandle hd
        , delegate_ctlc = True
        , env           = if null env then Nothing else Just env
        , cwd           = Just (depPath procDep)
        }

    env = map (Text.unpack *** Text.unpack) procEnv

    delay = threadDelay (procDelay * 1000)

terminate :: [Cmd] -> IO ()
terminate = mapM_ $ \c -> do
    interruptProcessGroupOf (cmdHd c) `catch` catchError ()
    waitProcess c

waitProcessAsync :: Cmd -> IO (Async (Cmd, ExitCode))
waitProcessAsync c@Cmd{..} = async $
    waitProcess c `catch` (\e -> printError e >> throwIO e)
  where
    printError :: AsyncException -> IO ()
    printError UserInterrupt = logDebugBS $ colourise cmdColour cmdPrefix "Ctrl-C"
    printError _             = return ()

waitProcess :: Cmd -> IO (Cmd, ExitCode)
waitProcess p@Cmd{..} = (p,) <$>
    waitForProcess cmdHd
        `catch` catchError (ExitFailure 123)
        `finally` do
            l <- poll cmdLog
            case l of
                Nothing         -> cancel cmdLog
                Just (Left ex)  -> syslogErr ex
                Just (Right ()) -> return ()
  where
    syslogErr ex
        | Just ThreadKilled <- fromException ex = return ()
        | otherwise = logError . C8.unpack
                    $ colourise cmdColour cmdPrefix
                    $ C8.pack (show ex)

catchError :: a -> SomeException -> IO a
catchError a _ = return a

connectToSyslog :: Socket
                -> Color
                -> Consumer ByteString IO ()
                -> Text
                -> IO (Async (), Handle)
connectToSyslog sock col out p = do
    l <- newEmptyMVar
    a <- async $ fork l
    (a,) <$> open <* takeMVar l
  where
    fork lock = do
        s <- fst <$> accept sock <* putMVar lock ()
        Conduit.sourceSocket s
            =$= Conduit.lines
            =$= Conduit.map (colourise col p)
             $$ Conduit.map (<> "\n")
            =$= out

    open = do
        cl <- socket AF_UNIX Stream defaultProtocol
        Socket.connect cl (SockAddrUnix syslogSock)
        hd <- socketToHandle cl WriteMode
        hSetBuffering hd LineBuffering
        return hd

syslogSock :: String
syslogSock = "keiretsu.syslog"

openSyslog :: IO Socket
openSyslog = open `catch` printError
  where
    open = do
        l <- socket AF_UNIX Stream defaultProtocol
        bind l (SockAddrUnix syslogSock)
        listen l 128
        return l

    printError :: SomeException -> a
    printError _ = error $ "Unable to open syslog socket '" ++ syslogSock ++ "'"

closeSyslog :: Socket -> IO ()
closeSyslog s = close s `finally` removeFile syslogSock
