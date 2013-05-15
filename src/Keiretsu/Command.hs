module Keiretsu.Command (
      start
    , retry
    , clean
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Monoid
import System.Posix.Signals

import qualified Keiretsu.Dependency  as Deps
import qualified Keiretsu.Environment as Env
import qualified Keiretsu.Process     as Procs

start :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> Bool -> IO ()
start cfg tmp envs verify build _conc = do
    putStrLn "[Config]"
    deps <- Deps.fromFile cfg tmp
    let slen = show (length deps) <> " dependencies."
    putStrLn $ "Loaded " <> slen

    when verify $ do
        putStrLn "[Verify]"
        mapM_ Deps.verify deps
        putStrLn $ "Verified " <> slen

    when build $ do
        putStrLn "[Build]"
        mapM_ Deps.build deps
        putStrLn $ "Built " <> slen

    putStrLn "[Procfiles]"
    procs <- Procs.load deps

    putStrLn "[Environments]"
    env <- Env.load envs

    putStrLn "[Handlers]"
    chan <- newChan
    mapM_ (handle chan) [sigINT, sigKILL, sigQUIT, sigTERM]

    putStrLn "[Processes]"
    Procs.start chan env procs >>= void . waitAnyCancel

    putStrLn "Exiting ..."

retry :: FilePath -> FilePath -> [FilePath] -> IO ()
retry cfg tmp envs = start cfg tmp envs False False False

clean :: FilePath -> FilePath -> Bool -> IO ()
clean cfg tmp force = do
    putStrLn "[Config]"
    deps <- Deps.fromFile cfg tmp
    mapM_ (if force then Deps.wipe else Deps.clean) deps

handle :: Chan Signal -> Signal -> IO ()
handle chan int = void $ installHandler int (Catch $ writeChan chan int) Nothing
