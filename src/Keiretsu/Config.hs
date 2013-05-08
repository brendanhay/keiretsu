module Keiretsu.Config (load) where

import Control.Applicative
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Text           (Text, pack)

import qualified Data.HashMap.Strict as M
import qualified Data.Yaml           as Y

load :: (Text -> Text -> a) -> FilePath -> IO [a]
load f path = map (uncurry f) . M.toList <$> loadYaml path

loadYaml :: FilePath -> IO (HashMap Text Text)
loadYaml path = maybe err (return . tmap) =<< Y.decodeFile path
  where
    err = error $ "Invalid config file: " <> path

    tmap (Y.Object m) = M.map conv m
    tmap other = error $ "Invalid config object: " <> show other

    conv (Y.String txt) = txt
    conv other = pack $ show other
