{-# LANGUAGE RecordWildCards      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Keiretsu.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Keiretsu.Types where

import           Control.Concurrent
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    (pack)
import qualified Data.ByteString.Char8    as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           System.Console.ANSI
import           System.Directory
import           System.Posix.Signals
import           System.Process
import           System.Process.Internals

type SignalChan = Chan (Signal, Maybe ProcessHandle)

instance Eq ProcessHandle where
    (ProcessHandle a) == (ProcessHandle b) = a == b

type Env = [(String, String)]

data Dep = Dep
    { depPath :: !FilePath
    , depName :: !String
    , depUri  :: !(Maybe String)
    } deriving (Eq, Show)

makeDep :: FilePath -> Maybe String -> Maybe String -> Dep
makeDep path mname = Dep path (fromMaybe (dirName path) mname)

makeLocalDep :: IO Dep
makeLocalDep = do
    dir <- getCurrentDirectory
    return $ makeDep dir Nothing Nothing

data Proc = Proc
    { procPath :: !FilePath
    , procName :: !String
    , procCmd  :: !String
    , procPort :: !(String, String)
    } deriving (Eq, Show)

makeProc :: Dep -> String -> String -> Word16 -> Proc
makeProc Dep{..} name cmd port =
    Proc depPath name cmd (portVar depName name, show port)

makeLocalProc :: String -> String -> IO Proc
makeLocalProc name cmd = do
    dir <- getCurrentDirectory
    return $ makeProc (makeDep dir (Just name) Nothing) name cmd 0

portVar :: String -> String -> String
portVar x y = map toUpper $ intercalate "_" [x, y, "PORT"]

data Cmd = Cmd
    { cmdPre   :: !String
    , cmdStr   :: !String
    , cmdDelay :: !Int
    , cmdDir   :: !(Maybe FilePath)
    , cmdEnv   :: !Env
    } deriving Show

makeCmds :: Env -> Int -> [Proc] -> [Cmd]
makeCmds env delay ps = map mk ps
  where
    mk Proc{..} = Cmd
        (dirName procPath <> "/" <> procName)
        procCmd
        delay
        (Just procPath)
        (("PORT", snd procPort) : procPort : env)

dirName :: FilePath -> String
dirName = BS.unpack . snd . BS.breakEnd (== '/') . BS.pack

colours :: [Color]
colours = cycle [Red, Green, Cyan, Yellow, Blue, Magenta, Cyan]

colourise :: Color -> ByteString -> ByteString -> ByteString
colourise c x y = BS.concat [pack prefix, x, pack suffix, y, pack clear]
  where
    prefix = setSGRCode [SetColor Foreground Vivid c, SetConsoleIntensity BoldIntensity]
    suffix = setSGRCode [SetColor Foreground Vivid c, SetConsoleIntensity NormalIntensity]
    clear  = setSGRCode []
