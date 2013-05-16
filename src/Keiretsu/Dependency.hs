module Keiretsu.Dependency (
      verify
    , build
    , wipe
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Keiretsu.Types
import System.Directory
import System.ShQQ

verify :: Dep -> IO ()
verify d = do
    p <- doesDirectoryExist $ depPath d
    (if p then update else clone) d

build :: Dep -> IO ()
build Dep{..} = do
    putStrLn $ "Building " <> depPath <> " ..."
    void [sh| cd $depPath && make install |]

update :: Dep -> IO ()
update d@Dep{..} = case depUri of
    Nothing -> return ()
    Just x  -> do
        p <- origin depPath x
        if p
         then do
             putStrLn $ "Updating " <> depPath <> " ..."
             void [sh| cd $depPath && git pull -f |]
         else clone d

origin :: FilePath -> String -> IO Bool
origin path uri = do
    p <- doesDirectoryExist path
    print p
    print uri
    c <- [sh| cd $path && git config --get remote.origin.uri |]
    print c
    if p
     then eq <$> [sh| cd $path && git config --get remote.origin.uri |]
     else return False
  where
    eq = (j uri ==) . j
    j  = unwords . lines

clone :: Dep -> IO ()
clone d@Dep{..} = case depUri of
    Nothing -> return ()
    Just x  -> do
        wipe True d
        putStrLn $ "Cloning " <> x <> " ..."
        void [sh| git clone $x $depPath |]

wipe :: Bool -> Dep -> IO ()
wipe True Dep{..} = do
    putStrLn $ "Wiping " <> depPath <> " ..."
    void [sh| rm -rf $depPath |]
wipe False Dep{..} = do
    putStrLn $ "Cleaning " <> depPath <> " ..."
    void [sh| cd $depPath && make clean |]
