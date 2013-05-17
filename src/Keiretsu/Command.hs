module Keiretsu.Command (
      clean
    , foreman
    , integrate
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Monoid
import Keiretsu.Config
import Keiretsu.Dependency
import Keiretsu.Process
import Keiretsu.Types
import System.Environment
import System.Exit
import System.Posix.Signals

clean :: FilePath -> FilePath -> Bool -> IO ()
clean cfg tmp force = readIntfile cfg tmp >>= mapM_ (wipe force)

foreman :: [FilePath] -> Bool -> IO ()
foreman paths fdump = do
    dep <- makeLocalDep
    ps  <- readProcfile dep
    env <- readEnvironments paths ps

    forkWait fdump . fst $ makeCmds colors env ps

integrate :: Bool
          -> FilePath
          -> FilePath
          -> [FilePath]
          -> Bool
          -> Bool
          -> Bool
          -> IO ()
integrate test cfg tmp paths fdump fverify fbuild = do
    ex   <- if test
             then sequence [makeLocalProc "test" "make test"]
             else return []

    ds   <- readIntfile cfg tmp

    whenFlag fverify verify ds
    whenFlag fbuild build ds

    ps   <- readProcfiles ds
    penv <- readEnvironments paths ps
    lenv <- getEnvironment

    let (disc, cs) = makeCmds colors penv ps
        (spec, _)  = makeCmds cs (penv ++ lenv) ex
        cmds = disc ++ spec

    forkWait fdump cmds

whenFlag :: Bool -> (a -> IO b) -> [a] -> IO ()
whenFlag p f = when p . void . mapConcurrently f

forkWait :: Bool -> [Cmd] -> IO ()
forkWait dump cs = do
    chan <- handleSignals
    runCommands dump chan cs >>= exitAfter

handleSignals :: IO SignalChan
handleSignals = do
    chan <- newChan
    mapM_ (signalHandler chan)
        [ sigINT
        , sigKILL
        , sigQUIT
        , sigTERM
        ]
    return chan

signalHandler :: SignalChan -> Signal -> IO ()
signalHandler chan sig =
    void $ installHandler sig (Catch $ writeChan chan (sig, Nothing)) Nothing

exitAfter :: [Async ExitCode] -> IO ()
exitAfter asyncs = do
    (_, code) <- waitAnyCancel asyncs
    threadDelay 10000
    putStrLn $ "Exiting with " <> show code <> " ..."
    putStrLn "Completed."
    exitWith code
