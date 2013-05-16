module Keiretsu.Command (
      foreman
    , integrate
    , test
    , clean
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Monoid
import Keiretsu.Config
import Keiretsu.Dependency
import Keiretsu.Process
import Keiretsu.Types
import System.Directory
import System.Exit
import System.Posix.Signals

foreman :: [FilePath] -> IO ()
foreman epaths = do
    dir <- getCurrentDirectory
    ps  <- readProcfile $ makeDep dir Nothing Nothing
    env <- readEnvironments epaths
    ch  <- newChan
    handleSignals ch
    forkProcesses ch env ps >>= exitAfter

integrate :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
integrate = run []

test :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
test cfg tmp epaths fverify fbuild = do
    dir <- getCurrentDirectory
    let p = makeProc (makeDep dir (Just "test") Nothing) "test" "make test" 1
    run [p] cfg tmp epaths fverify fbuild

clean :: FilePath -> FilePath -> Bool -> IO ()
clean cfg tmp force = readIntfile cfg tmp >>= mapM_ (wipe force)

run :: [Proc] -> FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
run extra cfg tmp epaths fverify fbuild = do
    ds  <- readIntfile cfg tmp
    when fverify $ mapM_ verify ds
    when fbuild . void $ mapConcurrently build ds
    ps  <- readProcfiles ds
    dir <- getCurrentDirectory
    env <- readEnvironments epaths
    ch  <- newChan
    handleSignals ch
    forkProcesses ch env (ps ++ extra) >>= exitAfter

handleSignals :: Chan Signal -> IO ()
handleSignals chan = do
    putStrLn "Installing Signal Handlers"
    mapM_ (signalHandler chan)
        [ sigINT
        , sigKILL
        , sigQUIT
        , sigTERM
        ]

signalHandler :: Chan Signal -> Signal -> IO ()
signalHandler chan int =
    void $ installHandler int (Catch $ writeChan chan int) Nothing

exitAfter :: [Async ExitCode] -> IO ()
exitAfter asyncs = do
    (_, code) <- waitAnyCancel asyncs
    threadDelay 10000
    putStrLn $ "Exiting with " <> show code <> " ..."
    putStrLn "Completed."
    exitWith code
