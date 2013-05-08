module Keiretsu.Command (
      start
    , clean
    ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import Data.Monoid

import qualified Keiretsu.Dependency as Deps
import qualified Keiretsu.Process    as Procs

start :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> Bool -> IO ()
start cfg tmp _envs build verify conc = do
    deps <- Deps.load cfg tmp

    let slen = show (length deps) <> " dependencies."
        map_ = if conc then mapConcurrently else mapM

    putStrLn $ "Loaded " <> slen

    when verify $ do
        putStrLn "Verifying dependencies ..."
        void $ map_ Deps.verify deps
        putStrLn $ "Verified " <> slen

    when build $ do
        putStrLn "Building dependencies ..."
        mapM_ Deps.build deps
        putStrLn $ "Built " <> slen

    putStrLn "Reading Procfiles ..."
    (procs, env) <- Procs.load deps

    putStrLn "Start dependencies ..."
    asyncs <- concat <$> mapM (Procs.start env) procs

    putStrLn "Waiting ..."
    void $ waitAnyCancel asyncs
    -- Trap any exit and run cleanup

clean :: FilePath -> FilePath -> Bool -> IO ()
clean cfg tmp force = do
    deps <- Deps.load cfg tmp
    mapM_ (if force then Deps.wipe else Deps.clean) deps
