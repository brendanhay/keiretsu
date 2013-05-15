module Keiretsu.Types where

import Data.ByteString (ByteString)
import Data.Word

type Spec = (ByteString, [Proc])

type Env = [(String, String)]

data Cmd = Cmd
    { cmdPre :: ByteString
    , cmdStr :: ByteString
    , cmdDir :: Maybe FilePath
    , cmdEnv :: Env
    } deriving (Eq, Show)

data Dep = Dep
    { depName :: ByteString
    , depPath :: FilePath
    , depUrl  :: String
    } deriving (Eq, Show)

data Proc = Proc
    { procName :: ByteString
    , procCmd  :: ByteString
    , procDir  :: FilePath
    , procVar  :: ByteString
    , procPort :: Word16
    } deriving (Eq, Show)
