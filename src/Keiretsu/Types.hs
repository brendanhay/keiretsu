{-# LANGUAGE RecordWildCards #-}

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
    return $ makeProc (makeDep (Just name) dir) name cmd 0

portVar :: String -> String -> String
portVar x y = map toUpper $ intercalate "_" [x, y, "PORT"]

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
        (dirName procPath <> "/" <> procName)
        procCmd
        delay
        (Just procPath)
        (("PORT", snd procPort) : procPort : env)

dirName :: FilePath -> String
dirName = BS.unpack . snd . BS.breakEnd (== '/') . BS.pack
