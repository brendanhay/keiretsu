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
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           System.Directory

type Env = [(String, String)]

data Dep = Dep
    { depName :: !String
    , depPath :: !FilePath
    } deriving (Eq, Show)

makeDep :: Maybe String -> FilePath -> Dep
makeDep name path = Dep (fromMaybe (dirName path) name) path

makeLocalDep :: IO Dep
makeLocalDep = makeDep Nothing <$> getCurrentDirectory

data Proc = Proc
    { procPath  :: !FilePath
    , procName  :: !String
    , procCmd   :: !String
    , procPorts :: [(String, String)]
    } deriving (Eq, Show)

makeProc :: Dep -> String -> String -> [Word16] -> Proc
makeProc Dep{..} name cmd ports = Proc depPath name cmd
   $ zipWith (portVars depName name) [0..] ports

makeLocalProc :: String -> String -> IO Proc
makeLocalProc name cmd = do
    dir <- getCurrentDirectory
    return $ makeProc (makeDep (Just name) dir) name cmd [0]

portVars :: String -> String -> Int -> Word16 -> (String, String)
portVars x y n (show -> p)
    | n == 0    = (name, p)
    | otherwise = (name ++ show n, p)
  where
    name = map toUpper . intercalate "_" $ [x, y, "PORT"]

data Cmd = Cmd
    { cmdPre   :: !String
    , cmdStr   :: !String
    , cmdDelay :: !Int
    , cmdDir   :: Maybe FilePath
    , cmdEnv   :: Env
    } deriving Show

makeCmds :: Env -> Int -> [Proc] -> [Cmd]
makeCmds env delay = map mk
  where
    mk Proc{..} = Cmd
        { cmdPre   = dirName procPath <> "/" <> procName
        , cmdStr   = procCmd
        , cmdDelay = delay
        , cmdDir   = Just procPath
        , cmdEnv   = port procPorts (procPorts ++ env)
        }

    -- FIXME: Brittle approach to finding the process' port.
    port []      = id
    port (p : _) = (("PORT", snd p) :)

dirName :: FilePath -> String
dirName = BS.unpack . snd . BS.breakEnd (== '/') . BS.pack
