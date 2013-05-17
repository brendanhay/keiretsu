module Keiretsu.Command (
      clean
    , start
    , integrate
    ) where

import Control.Applicative
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

start :: [FilePath] -> Bool -> IO ()
start paths fdump = do
    dep <- makeLocalDep
    ps  <- readProcfile dep
    env <- readEnvironments paths ps

    forkWait fdump . fst $ makeCmds colors env 0 ps

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

    let (disc, cs) = makeCmds colors penv 0 ps
        (spec, _)  = makeCmds cs (penv ++ lenv) delay ex
        cmds       = filter ((`notElem` excls) . cmdPre) $ disc ++ spec

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
