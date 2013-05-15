module Keiretsu.Types where

import Data.ByteString (ByteString)
import Data.Char
import Data.Monoid
import Data.Word

import qualified Data.ByteString.Char8 as BS

type Spec = (ByteString, [Proc])

type Env = [(String, String)]

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

makeProc :: Dep -> ByteString -> ByteString  -> Word16 -> Proc
makeProc d name cmd = Proc name cmd (depPath d) (portVar (depName d) name)

portVar :: ByteString -> ByteString -> ByteString
portVar x y = BS.map toUpper $ BS.intercalate "_" [x, y, "port"]

procDirName :: Proc -> ByteString
procDirName p =
    (snd . BS.breakEnd (== '/') . BS.pack $ procDir p) <> "/" <> procName p

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
    (procDirName p)
    (procCmd p)
    (Just $ procDir p)
    (procEnv p env)
