{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.List
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Word
import           System.Directory
import           System.FilePath

defaultDelay :: Int
defaultDelay = 10

defaultRetry :: Int
defaultRetry = 0

portRange :: [Int]
portRange = [1..]

data Dep = Dep
    { depPath  :: FilePath
    , depName  :: Text
    } deriving (Show, Eq)

instance FromJSON (Text -> Dep) where
    parseJSON = fmap Dep . withText "FilePath" (return . Text.unpack)

instance FromJSON [Dep] where
    parseJSON = withObject "Intfile" $ \o ->
        forM (Map.toList o) $ \(k, v) ->
            ($ k) <$> parseJSON v

depLocal :: IO Dep
depLocal = do
    d <- getCurrentDirectory
    return $! Dep d (nameFromDir d)

nameFromDir :: FilePath -> Text
nameFromDir = Text.pack . takeBaseName

type Env = [(Text, Text)]

data Port = Port
    { portLocal  :: Text
    , portRemote :: Text
    , portNumber :: !Word16
    } deriving (Eq, Show)

getPortEnv :: (Port -> Text) -> [Port] -> Env
getPortEnv f = map (f &&& Text.pack . show . portNumber)

newtype Time = Time Int
    deriving (Eq, Show)

data Proc = Proc
    { procEphem  :: !Bool
    , procName   :: Text
    , procCmd    :: Text
    , procCheck  :: Maybe Text
    , procDelay  :: !Int
    , procRetry  :: !Int
    , procEnv    :: Env
    , procPorts  :: [Port]
    , procPrefix :: Text
    , procDep    :: Dep
    } deriving (Show, Eq)

instance FromJSON [Dep -> Proc] where
    parseJSON = withObject "Procfile" $ \o ->
        forM (Map.toList o) $ \(k, v) -> do
            f <- foreman k v <|> keiretsu k v
            return $ \d ->
                f [] [] (nameFromDir (depPath d) <> "/" <> k) d
      where
        keiretsu k = withObject "Keiretsu Format" $ \o ->
            Proc False k
                <$> o .:  "command"
                <*> o .:? "check"
                <*> o .:? "delay" .!= defaultDelay
                <*> o .:? "retry" .!= defaultRetry

        foreman k = withText "Foreman Format" $ \cmd ->
            return $ Proc False k cmd Nothing defaultDelay defaultRetry

procLocal :: Dep -> Env -> Int -> Int -> Text -> Proc
procLocal d e n i c =
    let t = "run"
        p = t <> "-" <> Text.pack (show i)
     in Proc True t c Nothing n defaultRetry e [] p d

getEnvFiles :: [Proc] -> [FilePath]
getEnvFiles = nub . map ((</> ".env") . depPath . procDep)

setLocalEnv :: Env -> Proc -> Proc
setLocalEnv e p = p { procEnv = getPortEnv portLocal (procPorts p) ++ e }

getRemoteEnv :: [Proc] -> Env
getRemoteEnv = concatMap (getPortEnv portRemote . procPorts)

excludeProcs :: [Text] -> [Proc] -> [Proc]
excludeProcs xs = filter ((`notElem` xs) . procPrefix)
