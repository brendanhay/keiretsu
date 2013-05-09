module Keiretsu.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text              (Text)
import Data.Word
import System.Directory
import System.FilePath
import System.ShQQ

import qualified Data.Text       as T
import qualified Keiretsu.Config as Cfg

type Spec = (Text, [Proc])

type Logger = String -> IO ()

type Env = [(String, String)]

data Dep = Dep
    { depName :: Text
    , depPath :: FilePath
    , depUrl  :: String
    } deriving (Eq, Show)

data Proc = Proc
    { procName :: Text
    , procCmd  :: Text
    , procDir  :: FilePath
    , procVar  :: Text
    , procPort :: Word16
    } deriving (Eq, Show)
