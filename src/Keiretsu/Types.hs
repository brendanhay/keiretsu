module Keiretsu.Types where

import Data.ByteString (ByteString)
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Word
import System.Exit

import qualified Data.ByteString.Char8 as BS

type Env = [(String, String)]

data Dep = Dep
    { depName :: ByteString
    , depPath :: FilePath
    , depUri  :: Maybe String
    } deriving (Eq, Show)

makeDep :: FilePath -> Maybe ByteString -> Maybe String -> Dep
makeDep path mname = Dep (fromMaybe (dirName path) mname) path

data Proc = Proc
    { procName :: ByteString
    , procCmd  :: ByteString
    , procDir  :: FilePath
    , procVar  :: ByteString
    , procPort :: Word16
    } deriving (Eq, Show)

makeProc :: Dep -> ByteString -> ByteString  -> Word16 -> Proc
makeProc d name cmd = Proc name cmd (depPath d) (portVar (depName d) name)

portVar :: ByteString -> ByteString -> ByteString
portVar x y = BS.map toUpper $ BS.intercalate "_" [x, y, "port"]

procEnv :: Proc -> Env -> Env
procEnv p = (("PORT", show $ procPort p) :)

data Cmd = Cmd
    { cmdPre :: ByteString
    , cmdStr :: ByteString
    , cmdDir :: Maybe FilePath
    , cmdEnv :: Env
    } deriving (Eq, Show)

makeCmd :: Proc -> Env -> Cmd
makeCmd p env = Cmd
    (dirName (procDir p) <> "/" <> procName p)
    (procCmd p)
    (Just $ procDir p)
    (procEnv p env)

dirName :: FilePath -> ByteString
dirName = snd . BS.breakEnd (== '/') . BS.pack
