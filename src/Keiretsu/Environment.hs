module Keiretsu.Environment (
      fromProcs
    , fromFiles
    ) where

import Control.Arrow
import Data.Monoid
import Keiretsu.Types
import System.Directory

import qualified Data.Text as T

local :: FilePath
local = "./.env"

fromProcs :: [Proc] -> Env
fromProcs = map (\Proc{..} -> (T.unpack procVar, show procPort))

fromFiles :: [FilePath] -> IO Env
fromFiles paths = do
    p    <- doesFileExist local
    strs <- mapM f $ if p then local : paths else paths
    return . map (second tail . break (== '=')) . lines $ concat strs
  where
    f x = putStrLn ("Reading " <> x <> " ...") >> readFile x
