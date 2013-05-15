module Keiretsu.Config (load) where

import Control.Applicative
import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text           (Text)
import Data.Monoid

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding    as E
import qualified Data.HashMap.Strict   as M
import qualified Data.Yaml             as Y

load :: (ByteString -> ByteString -> a) -> FilePath -> IO [a]
load f path = map g . M.toList <$> loadYaml path
  where
    g (k, v) = f (E.encodeUtf8 k) v

loadYaml :: FilePath -> IO (HashMap Text ByteString)
loadYaml path = maybe err (return . tmap) =<< Y.decodeFile path
  where
    err = error $ "Invalid config file: " <> path

    tmap (Y.Object m) = M.map conv m
    tmap other        = error $ "Invalid config object: " <> show other

    conv (Y.String t) = E.encodeUtf8 t
    conv other        = BS.pack $ show other
