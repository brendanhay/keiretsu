{-# OPTIONS_GHC -fno-warn-orphans #-}

module Keiretsu.Types where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Word
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
    , depName :: ByteString
    , depUri  :: Maybe String
    } deriving (Eq, Show)

makeDep :: FilePath -> Maybe ByteString -> Maybe String -> Dep
makeDep path mname = Dep path (fromMaybe (dirName path) mname)

makeLocalDep :: IO Dep
makeLocalDep = do
    dir <- getCurrentDirectory
    return $ makeDep dir Nothing Nothing

data Proc = Proc
    { procPath :: FilePath
    , procName :: ByteString
    , procCmd  :: ByteString
    , procPort :: (String, String)
    } deriving (Eq, Show)

makeProc :: Dep -> ByteString -> ByteString -> Word16 -> Proc
makeProc Dep{..} name cmd port =
    Proc depPath name cmd (portVar depName name, show port)

makeLocalProc :: ByteString -> ByteString -> IO Proc
makeLocalProc name cmd = do
    dir <- getCurrentDirectory
    return $ makeProc (makeDep dir (Just name) Nothing) name cmd 0

portVar :: ByteString -> ByteString -> String
portVar x y =
    BS.unpack . BS.map toUpper $ BS.intercalate "_" [x, y, "PORT"]

data Cmd = Cmd
    { cmdPre :: ByteString
    , cmdStr :: ByteString
    , cmdDir :: Maybe FilePath
    , cmdEnv :: Env
    } deriving (Eq, Show)

makeCmd :: Env -> Proc -> Cmd
makeCmd env Proc{..} = Cmd
    (dirName procPath <> "/" <> procName)
    procCmd
    (Just procPath)
    (("PORT", snd procPort) : procPort : env)

makeCmds :: Env -> [Proc] -> [Cmd]
makeCmds env = map (makeCmd env)

dirName :: FilePath -> ByteString
dirName = snd . BS.breakEnd (== '/') . BS.pack
