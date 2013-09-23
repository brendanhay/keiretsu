-- Module      : Keiretsu.Config
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Keiretsu.Config
    ( loadDeps
    , readEnvs
    , readProcs
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception     (bracket)
import           Control.Monad
import qualified Data.Attoparsec       as P
import qualified Data.Attoparsec.Char8 as P8
import qualified Data.ByteString.Char8 as BS
import           Data.Function
import           Data.List
import qualified Data.Set as Set
import           Keiretsu.Log
import           Keiretsu.Types
import           Network.Socket
import           System.Directory
import           System.FilePath

loadDeps :: FilePath -> IO [Dep]
loadDeps rel = nub <$> loadDeps' Set.empty rel
  where
    loadDeps' memo rel' = do
        dir <- canonicalizePath rel'
        let path = dir </> "Intfile"
        if path `Set.notMember` memo
            then do
                logDebug $ "Loading " ++ dir ++ " ..."
                with dir $ load path memo =<< doesFileExist path
            else return []

    load _ _ False = return []
    load path memo True = do
        logDebug $ "Reading " ++ path ++ " ..."
        p  <- mapM (uncurry dep) =<< readConfig (,) path
        cs <- concat <$> mapM (loadDeps' (Set.insert path memo) . depPath) p
        return $! p ++ cs

    dep name = fmap (makeDep (Just name)) . canonicalizePath

    with path f = bracket getCurrentDirectory setCurrentDirectory
      (const $ setCurrentDirectory path >> f)

readEnvs :: [Dep] -> [FilePath] -> [Proc] -> IO Env
readEnvs ds fs ps = do
    paths <- filterM doesFileExist $ map ((</> ".env") . depPath) ds
    env   <- mapM read' $ paths ++ fs
    return $! merge (parse env) ps
  where
    read' path = logDebug ("Reading " ++ path ++ " ...") >> readFile path

    merge env = nubBy ((==) `on` fst) . (++ env) . map procPort
    parse     = map (second tail . break (== '=')) . lines . concat

readProcs :: [Dep] -> IO [Proc]
readProcs = liftM concat . mapM readProcfile
  where
    readProcfile d = do
        let cfg = joinPath [depPath d, "Procfile"]
        logDebug $ "Reading " ++ cfg ++ " ..."
        xs <- readConfig (makeProc d) cfg
        ys <- freePorts $ length xs
        return $! zipWith ($) xs ys

    freePorts n = do
        ss <- sequence . take n $ repeat assignSocket
        ps <- mapM ((fromIntegral <$>) . socketPort) ss
        mapM_ close ss
        return ps

    assignSocket = do
        s <- socket AF_INET Stream defaultProtocol
        a <- inet_addr "127.0.0.1"
        setSocketOption s NoDelay 0
        bind s $ SockAddrInet aNY_PORT a
        return s

readConfig :: (String -> String -> a) -> FilePath -> IO [a]
readConfig f path =
    either error return . P8.parseOnly parser =<< BS.readFile path
  where
    parser = P.many' $ do
        k <- P8.takeWhile1 (/= ':') <* P8.char ':' <* P8.takeWhile P8.isSpace
        v <- P.takeTill P8.isEndOfLine <* P8.endOfLine
        return $! f (BS.unpack k) (BS.unpack v)
