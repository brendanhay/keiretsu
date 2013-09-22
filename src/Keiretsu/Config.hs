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
import           Control.Exception   (bracket)
import           Control.Monad
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Yaml           as Yaml
import           Keiretsu.Types
import           Network.Socket
import           System.Directory
import           System.FilePath

loadDeps :: FilePath -> IO [Dep]
loadDeps rel = do
    dir <- canonicalizePath rel
    withCWD dir $ do
        let path = dir </> "Intfile"
        putStrLn $ "Loading " ++ dir ++ " ..."
        load path =<< doesFileExist path
  where
    load _    False = return []
    load path True  = do
        putStrLn ("Reading " ++ path ++ " ...")
        p  <- mapM (uncurry dep) =<< readConfig (,) path
        cs <- concat <$> mapM (loadDeps . depPath) p
        return $! p ++ cs

    dep name path = makeDep (Just name) <$> canonicalizePath path

readEnvs :: [Dep] -> [FilePath] -> [Proc] -> IO Env
readEnvs ds fs ps = do
    paths <- filterM doesFileExist $ map ((</> ".env") . depPath) ds
    env   <- mapM read' $ paths ++ fs
    return $! merge (parse env) ps
  where
    read' path = putStrLn ("Reading " ++ path ++ " ...") >> readFile path
    merge env  = nubBy ((==) `on` fst) . (++ env) . map procPort
    parse      = map (second tail . break (== '=')) . lines . concat

readProcs :: [Dep] -> IO [Proc]
readProcs = liftM concat . mapM readProcfile
  where
    readProcfile d = do
        let cfg = joinPath [depPath d, "Procfile"]
        putStrLn $ "Reading " ++ cfg ++ " ..."
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
    map (\(k, v) -> f (Text.unpack k) v) . Map.toList <$> loadYAML path

loadYAML :: FilePath -> IO (HashMap Text String)
loadYAML path = do
    isFile <- doesFileExist path
    if isFile
        then maybe err (return . tmap) =<< Yaml.decodeFile path
        else return Map.empty
  where
    err = error $ "Invalid config file: " ++ path

    tmap (Yaml.Object m) = Map.map conv m
    tmap other           = error $ "Invalid config object: " ++ show other

    conv (Yaml.String t) = Text.unpack t
    conv other           = show other

withCWD :: FilePath -> IO a -> IO a
withCWD path f = bracket getCurrentDirectory setCurrentDirectory
    (const $ setCurrentDirectory path >> f)
