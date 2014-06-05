{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict as Map
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Word
import           System.Directory
import           System.FilePath

defaultDelay :: Int
defaultDelay = 1000

portRange :: [Int]
portRange = [1..]

type Env = [(Text, Text)]

data Port = Port
    { portLocal  :: Text
    , portRemote :: Text
    , portNumber :: !Word16
    } deriving (Eq, Show)

getPortEnv :: (Port -> Text) -> [Port] -> Env
getPortEnv f = map (f &&& Text.pack . show . portNumber)

data Proc = Proc
    { procName   :: Text
    , procCmd    :: Text
    , procEnv    :: Env
    , procPorts  :: [Port]
    , procPrefix :: Text
    , procDep    :: Dep
    } deriving (Show, Eq)

instance FromJSON [Dep -> Proc] where
    parseJSON = withObject "Procfile" $ \o ->
        forM (Map.toList o) $ \(k, v) -> do
            f <- Proc k <$> withText "Command" return v <*> pure [] <*> pure []
            return $ \d ->
                f (nameFromDir (depPath d) <> "/" <> k) d

setProcEnv :: Env -> Proc -> Proc
setProcEnv e p = p { procEnv = getPortEnv portLocal (procPorts p) ++ e }

data Dep = Dep
    { depDelay :: !Int
    , depCheck :: Maybe Text
    , depPath  :: FilePath
    , depProcs :: [Proc]
    , depName  :: Text
    } deriving (Show, Eq)

instance FromJSON (Text -> Dep) where
    parseJSON = withObject "Dependency" $ \o ->
        Dep <$> o .:? "delay" .!= defaultDelay
            <*> o .:? "check"
            <*> o .:  "path"
            <*> pure []

instance FromJSON [Dep] where
    parseJSON = withObject "Intfile" $ \o ->
        forM (Map.toList o) $ \(k, v) ->
            ($ k) <$> parseJSON v

depLocal :: IO Dep
depLocal = do
    d <- getCurrentDirectory
    return $! Dep defaultDelay Nothing d [] (nameFromDir d)

getDepEnv :: Dep -> Env
getDepEnv = concatMap (getPortEnv portRemote . procPorts) . depProcs

setDepEnv :: Env -> Dep -> Dep
setDepEnv e d = d { depProcs = map (setProcEnv e) (depProcs d) }

exclude :: [Text] -> Dep -> Dep
exclude xs d = d { depProcs = filter ((`notElem` xs) . procPrefix) (depProcs d) }

nameFromDir :: FilePath -> Text
nameFromDir = Text.pack . takeBaseName
