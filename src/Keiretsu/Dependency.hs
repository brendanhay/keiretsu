module Keiretsu.Dependency (
      Dep(..)
    , load
    , verify
    , build
    , wipe
    , clean
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Text            (Text)
import System.Directory
import System.FilePath
import System.ShQQ

import qualified Data.Text        as T
import qualified Keiretsu.Config as Cfg

data Dep = Dep
    { depName :: Text
    , depPath :: FilePath
    , depUrl  :: String
    } deriving (Eq, Show)

load :: FilePath -> FilePath -> IO [Dep]
load cfg tmp = do
    putStrLn $ "Loading " <> cfg <> " ..."
    Cfg.load (\k -> Dep k (joinPath [tmp, T.unpack k]) . T.unpack) cfg

verify :: Dep -> IO ()
verify d = do
    p <- doesDirectoryExist $ depPath d
    (if p then update else clone) d

build :: Dep -> IO ()
build Dep{..} = do
    putStrLn $ "Building " <> depPath <> " ..."
    void [sh| cd $depPath && make install |]

update :: Dep -> IO ()
update d@Dep{..} = do
    p <- origin d
    if p
     then do
         putStrLn $ "Updating " <> depPath <> " ..."
         void [sh| cd $depPath && git pull -f $+redirect |]
     else clone d

origin :: Dep -> IO Bool
origin Dep{..} = do
    p <- doesDirectoryExist depPath
    if p
     then eq <$> [sh| cd $depPath && git config --get remote.origin.url |]
     else return False
  where
    eq = (j depUrl ==) . j
    j  = unwords . lines

clone :: Dep -> IO ()
clone d@Dep{..} = do
    wipe d
    putStrLn $ "Cloning " <> depUrl <> " ..."
    void [sh| git clone $depUrl $depPath $+redirect |]

wipe :: Dep -> IO ()
wipe Dep{..} = do
    putStrLn $ "Wiping " <> depPath <> " ..."
    void [sh| rm -rf $depPath |]

clean :: Dep -> IO ()
clean Dep{..} = do
    putStrLn $ "Cleaning " <> depPath <> " ..."
    void [sh| cd $depPath && make clean |]

redirect :: String
redirect = "> /dev/null 2>&1"
