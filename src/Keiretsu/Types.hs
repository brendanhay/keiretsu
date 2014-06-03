{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

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

import           Control.Applicative
import           Control.Arrow
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           System.Directory

type Env = [(String, String)]

data Dep = Dep
    { depName :: String
    , depPath :: FilePath
    } deriving (Eq, Show)

makeDep :: Maybe String -> FilePath -> Dep
makeDep name path = Dep (fromMaybe (dirName path) name) path

makeLocalDep :: IO Dep
makeLocalDep = makeDep Nothing <$> getCurrentDirectory

data Port = Port
    { portLocal  :: String
    , portRemote :: String
    , portNumber :: !Word16
    } deriving (Eq, Show)

data Proc = Proc
    { procPath  :: FilePath
    , procName  :: String
    , procCmd   :: String
    , procPorts :: [Port]
    } deriving (Eq, Show)

makeProc :: Dep -> String -> String -> [Word16] -> Proc
makeProc Dep{..} name cmd = Proc depPath name cmd . zipWith vars portRange
  where
    vars n p
        | n == lower = Port local remote p
        | otherwise  = let s = show n in Port (local ++ s) (remote ++ s) p

    lower  = head portRange
    remote = map toUpper $ intercalate "_" [depName, name, local]
    local  = "PORT"

makeLocalProc :: String -> String -> IO Proc
makeLocalProc name cmd = do
    dir <- getCurrentDirectory
    return $ makeProc (makeDep (Just name) dir) name cmd [0]

localPortEnv :: Proc -> Env
localPortEnv = map (portLocal &&& show . portNumber) . procPorts

remotePortEnv :: Proc -> Env
remotePortEnv = map (portRemote &&& show . portNumber) . procPorts

portRange :: [Int]
portRange = [1..]

data Cmd = Cmd
    { cmdPre   :: String
    , cmdStr   :: String
    , cmdDelay :: !Int
    , cmdDir   :: Maybe FilePath
    , cmdEnv   :: Env
    } deriving Show

makeCmds :: Env -> Int -> [Proc] -> [Cmd]
makeCmds env delay = map mk
  where
    mk p@Proc{..} = Cmd
        { cmdPre   = dirName procPath <> "/" <> procName
        , cmdStr   = procCmd
        , cmdDelay = delay
        , cmdDir   = Just procPath
        , cmdEnv   = localPortEnv p ++ env
        }

dirName :: FilePath -> String
dirName = BS.unpack . snd . BS.breakEnd (== '/') . BS.pack
