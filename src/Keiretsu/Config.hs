module Keiretsu.Config (
      readEnvironments
    , readIntfile
    , readProcfiles
    , readProcfile
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Function
import Data.List
import Data.Monoid
import Data.Text           (Text)
import Data.Word
import Keiretsu.Types
import Network.Socket
import System.Directory
import System.FilePath

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding    as E
import qualified Data.HashMap.Strict   as M
import qualified Data.Yaml             as Y

local :: FilePath
local = "./.env"

readEnvironments :: [FilePath] -> [Proc] -> IO Env
readEnvironments paths ps = do
    p <- doesFileExist local
    s <- mapM f $ if p then local : paths else paths
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
    f k = makeDep (joinPath [tmp, BS.unpack k]) (Just k) . Just . BS.unpack

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

readConfig :: (ByteString -> ByteString -> a) -> FilePath -> IO [a]
readConfig f path =
    map (\(k, v) -> f (E.encodeUtf8 k) v) . M.toList <$> loadYaml path

loadYaml :: FilePath -> IO (HashMap Text ByteString)
loadYaml path = maybe err (return . tmap) =<< Y.decodeFile path
  where
    err = error $ "Invalid config file: " <> path

    tmap (Y.Object m) = M.map conv m
    tmap other        = error $ "Invalid config object: " <> show other

    conv (Y.String t) = E.encodeUtf8 t
    conv other        = BS.pack $ show other

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
    bind s $ SockAddrInet aNY_PORT a
    return s
