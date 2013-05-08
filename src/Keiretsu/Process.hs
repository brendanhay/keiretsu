module Keiretsu.Process (
      Proc(..)
    , load
    , start
    ) where

import Control.Applicative
import Control.Concurrent.Async
import Data.Monoid
import Data.Text                (Text)
import Data.Word
import Network.Socket
import Keiretsu.Dependency      (Dep(..))
import System.FilePath
import System.Exit
import System.Process

import qualified Data.Text       as T
import qualified Keiretsu.Config as Cfg

data Proc = Proc
    { procName :: Text
    , procCmd  :: Text
    , procDir  :: FilePath
    , procVar  :: Text
    , procPort :: Word16
    } deriving (Eq, Show)

type Spec = (Text, [Proc])
type Env  = [(String, String)]

load :: [Dep] -> IO ([Spec], Env)
load deps = do
    procs <- mapM loadOne deps
    return (procs, environment $ concatMap snd procs)

start :: Env -> Spec -> IO [Async ExitCode]
start env (name, procs) = do
    putStrLn $ "Starting " <> T.unpack name <> " processes ..."
    mapM (startOne env) procs

environment :: [Proc] -> Env
environment = map (\Proc{..} -> (T.unpack procVar, show procPort))

loadOne :: Dep -> IO Spec
loadOne Dep{..} = do
    putStrLn $ "Loading " <> cfg <> " ..."
    xs <- Cfg.load mk cfg
    ys <- findPorts $ length xs
    return (depName, zipWith ($) xs ys)
  where
    cfg = joinPath [depPath, "Procfile"]
    mk k v = Proc k v depPath (portVar depName k)

startOne :: Env -> Proc -> IO (Async ExitCode)
startOne env Proc{..} = do
    putStrLn . init $ unlines
        [ "Forking " <> T.unpack procName <> " ..."
        , " Cmd: " <> cmd
        , " Dir: " <> procDir
        , " Env: " <> show env'
        ]
    async $ withEnv cmd procDir env'
  where
    cmd  = T.unpack procCmd
    env' = ("PORT", show procPort) : env

portVar :: Text -> Text -> Text
portVar x y = T.toUpper $ T.intercalate "_" [x, y, "port"]

findPorts :: Int -> IO [Word16]
findPorts n = do
    ss <- sequence . take n $ repeat findSocket
    ps <- mapM ((fromIntegral <$>) . socketPort) ss
    mapM_ close ss
    return ps

findSocket :: IO Socket
findSocket = do
    sock <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "127.0.0.1"
    bind sock $ SockAddrInet aNY_PORT addr
    return sock

withEnv :: String -> FilePath -> [(String, String)] -> IO ExitCode
withEnv s cwd env = do
    (_, _, _, pid) <- createProcess $ (shell s)
        { cwd = Just cwd
        , env = Just env
        }
    waitForProcess pid
