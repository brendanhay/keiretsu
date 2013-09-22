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
    ( readEnvironments
    , readIntfile
    , readProcfiles
    , readProcfile
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Word
import qualified Data.Yaml           as Y
import           Keiretsu.Types
import           Network.Socket
import           System.Directory
import           System.FilePath

local :: FilePath
local = "./.env"

readEnvironments :: [FilePath] -> [Proc] -> IO Env
readEnvironments paths ps = do
    p <- doesFileExist local
    s <- mapM f $ if p then paths ++ [local] else paths
    return $! mergeEnvironment (parseEnvironment s) ps
  where
    f x = putStrLn ("Reading " <> x <> " ...") >> readFile x

parseEnvironment :: [String] -> Env
parseEnvironment =
    map (second tail . break (== '=')) . lines . concat

mergeEnvironment :: Env -> [Proc] -> Env
mergeEnvironment env =
    nubBy ((==) `on` fst) . (++ env) . map procPort

readIntfile :: FilePath -> FilePath -> IO [Dep]
readIntfile cfg tmp = do
    putStrLn $ "Reading " <> cfg <> " ..."
    readConfig f cfg
  where
    f k = makeDep (joinPath [tmp, k]) (Just k) . Just

readProcfiles :: [Dep] -> IO [Proc]
readProcfiles = liftM concat . mapM readProcfile

readProcfile :: Dep -> IO [Proc]
readProcfile d = do
    putStrLn $ "Reading " <> cfg <> " ..."
    xs <- readConfig (makeProc d) cfg
    ys <- freePorts $ length xs
    return $ zipWith ($) xs ys
  where
    cfg = joinPath [depPath d, "Procfile"]

readConfig :: (String -> String -> a) -> FilePath -> IO [a]
readConfig f path =
    map (\(k, v) -> f (T.unpack k) v) . M.toList <$> loadYaml path

loadYaml :: FilePath -> IO (HashMap Text String)
loadYaml path = do
    isFile <- doesFileExist path
    if isFile
        then maybe err (return . tmap) =<< Y.decodeFile path
        else return M.empty
  where
    err = error $ "Invalid config file: " <> path

    tmap (Y.Object m) = M.map conv m
    tmap other        = error $ "Invalid config object: " <> show other

    conv (Y.String t) = T.unpack t
    conv other        = show other

freePorts :: Int -> IO [Word16]
freePorts n = do
    ss <- sequence . take n $ repeat assignSocket
    ps <- mapM ((fromIntegral <$>) . socketPort) ss
    mapM_ close ss
    return ps

assignSocket :: IO Socket
assignSocket = do
    s <- socket AF_INET Stream defaultProtocol
    a <- inet_addr "127.0.0.1"
    setSocketOption s NoDelay 0
    bind s $ SockAddrInet aNY_PORT a
    return s
