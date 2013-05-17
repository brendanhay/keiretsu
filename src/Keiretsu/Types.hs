{-# OPTIONS_GHC -fno-warn-orphans #-}

module Keiretsu.Types where

import Control.Concurrent
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Word
import System.Console.Rainbow
import System.Directory
import System.Posix.Signals
import System.Process
import System.Process.Internals

import qualified Data.ByteString.Char8 as BS

type SignalChan = Chan (Signal, Maybe ProcessHandle)

-- Orphan instance
instance Eq ProcessHandle where
    (ProcessHandle a) == (ProcessHandle b) = a == b

type Env = [(String, String)]

data Dep = Dep
    { depPath :: FilePath
    , depName :: String
    , depUri  :: Maybe String
    } deriving (Eq, Show)

makeDep :: FilePath -> Maybe String -> Maybe String -> Dep
makeDep path mname = Dep path (fromMaybe (dirName path) mname)

makeLocalDep :: IO Dep
makeLocalDep = do
    dir <- getCurrentDirectory
    return $ makeDep dir Nothing Nothing

data Proc = Proc
    { procPath :: FilePath
    , procName :: String
    , procCmd  :: String
    , procPort :: (String, String)
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
    { cmdPre   :: String
    , cmdStr   :: String
    , cmdDir   :: Maybe FilePath
    , cmdEnv   :: Env
    , cmdColor :: ForegroundAll
    }

colors :: [ForegroundAll]
colors = cycle
    [ f_red
    , f_green
    , f_yellow
    , f_blue
    , f_magenta
    , f_cyan
    , f_white
    ]

makeCmds :: [ForegroundAll] -> Env -> [Proc] -> ([Cmd], [ForegroundAll])
makeCmds cs env ps = (zipWith mk ps cs', rst)
  where
    (cs', rst) = splitAt (length ps) cs
    mk Proc{..} = Cmd
        (dirName procPath <> "/" <> procName)
        procCmd
        (Just procPath)
        (("PORT", snd procPort) : procPort : env)

dirName :: FilePath -> String
dirName = BS.unpack . snd . BS.breakEnd (== '/') . BS.pack
