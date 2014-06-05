{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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
   ( dependencies
   , proctypes
   , environment
   ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception     (bracket)
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Function
import qualified Data.HashSet          as Set
import           Data.List
import           Data.Monoid
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import           Data.Yaml
import           Keiretsu.Log
import           Keiretsu.Types
import           Network.Socket
import           System.Directory
import           System.FilePath

dependencies :: FilePath -> IO [Dep]
dependencies = fmap (reverse . nub) . go mempty <=< canonicalizePath
  where
    go memo dir = do
        let path = dir </> "Intfile"
        if not (path `Set.member` memo)
            then do
                logDebug $ "Loading " ++ dir ++ " ..."
                p <- doesFileExist path
                with dir $
                    load memo path p
            else do
                logDebug $ "Already loaded " ++ dir ++ ", skipping."
                return []

    with path f =
        bracket getCurrentDirectory setCurrentDirectory
            (const $ setCurrentDirectory path >> f)

    load _    _    False = return []
    load memo path True  = do
        logDebug $ "Reading " ++ path ++ " ..."
        xs <- decodeYAML path >>= mapM canonicalise
        ys <- concat <$> mapM (go (Set.insert path memo) . depPath) xs
        return $! xs ++ ys

    canonicalise d = (\p -> d { depPath = p }) <$> canonicalizePath (depPath d)

proctypes :: Int -> Dep -> IO [Proc]
proctypes n d@Dep{..} = do
    logDebug $ "Reading " ++ path ++ " ..."
    fs <- decodeYAML path
    ws <- alloc (length fs)
    return $! reverse $ zipWith proc' fs ws
  where
    path = depPath </> "Procfile"

    alloc m = do
        !ss <- replicateM m . sequence $ replicate n sock
        forM ss . mapM $ \s -> fromIntegral <$> socketPort s <* close s

    sock = do
        s <- socket AF_INET Stream defaultProtocol
        a <- inet_addr "127.0.0.1"
        setSocketOption s NoDelay 0
        bind s $ SockAddrInet aNY_PORT a
        return s

    proc' (($ d) -> p@Proc{..}) ws = p
        { procPorts = zipWith ports portRange ws
        }
      where
        ports i w
            | i == bound = Port local remote w
            | otherwise  =
                let s = Text.pack (show i)
                 in Port (local <> s) (remote <> s) w

        bound  = head portRange
        remote = Text.toUpper $ Text.intercalate "_" [depName, procName, local]
        local  = "PORT"

environment :: [Proc] -> [FilePath] -> IO Env
environment ps fs = do
    es  <- filterM doesFileExist $ getEnvFiles ps
    env <- mapM read' $ es ++ fs
    return $! merge (parse env) ps
  where
    read' path = logDebug ("Reading " ++ path ++ " ...") >> Text.readFile path

    merge env = nubBy ((==) `on` fst)
        . (++ env)
        . getRemoteEnv

    parse = map (second Text.tail . Text.break (== '='))
        . Text.lines
        . Text.concat

decodeYAML :: FromJSON a => FilePath -> IO a
decodeYAML = either error return . decodeEither <=< BS.readFile
