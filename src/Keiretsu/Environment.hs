module Keiretsu.Environment (
      load
    , merge
    ) where

import Control.Arrow
import Data.Function
import Data.List
import Data.Monoid
import Keiretsu.Types
import System.Directory

import qualified Data.ByteString.Char8 as BS

local :: FilePath
local = "./.env"

load :: [FilePath] -> IO Env
load paths = do
    p    <- doesFileExist local
    strs <- mapM f $ if p then local : paths else paths
    return . map (second tail . break (== '=')) . lines $ concat strs
  where
    f x = putStrLn ("Reading " <> x <> " ...") >> readFile x

merge :: Env -> [Proc] -> Env
merge env = nubBy ((==) `on` fst) . (++ env) . f
  where
    f = map (\Proc{..} -> (BS.unpack procVar, show procPort))
