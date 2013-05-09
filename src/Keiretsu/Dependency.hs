module Keiretsu.Dependency (
      fromFile
    , verify
    , build
    , wipe
    , clean
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Keiretsu.Types
import System.Directory
import System.FilePath
import System.ShQQ

import qualified Data.Text       as T
import qualified Keiretsu.Config as Cfg

fromFile :: FilePath -> FilePath -> IO [Dep]
fromFile cfg tmp = do
    putStrLn $ "Loading " <> cfg <> " ..."
    liftIO $ Cfg.load f cfg
  where
    f k = Dep k (joinPath [tmp, T.unpack k]) . T.unpack

verify :: Dep -> IO ()
verify d = do
    p <- liftIO . doesDirectoryExist $ depPath d
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
         void [sh| cd $depPath && git pull -f |]
     else clone d

origin :: Dep -> IO Bool
origin Dep{..} = do
    p <- liftIO $ doesDirectoryExist depPath
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
redirect = " > /dev/null 2>&1"
