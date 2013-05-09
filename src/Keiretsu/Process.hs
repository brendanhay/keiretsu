module Keiretsu.Process (
      fromDependencies
    , start
    ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.List
import Data.Monoid
import Data.Text                (Text)
import Data.Word
import Network.Socket
import Keiretsu.Environment
import Keiretsu.Terminal
import Keiretsu.Types
import System.FilePath
import System.Exit

import qualified Data.Text       as T
import qualified Keiretsu.Config as Cfg

fromDependencies :: [Dep] -> IO [Spec]
fromDependencies = mapM f
  where
    f Dep{..} = do
        putStrLn $ "Loading " <> cfg <> " ..."
        xs <- liftIO $ Cfg.load mk cfg
        ys <- liftIO . findPorts $ length xs
        return (depName, zipWith ($) xs ys)
      where
        cfg = joinPath [depPath, "Procfile"]
        mk k v = Proc k v depPath (portVar depName k)

start :: Env -> [Spec] -> IO [Async ExitCode]
start env specs = do
    logger <- getLogger
    asyncs <- mapM (startSpec env' logger) specs
    return $ concat asyncs
  where
    env' = nubBy uniq $ fromProcs (concatMap snd specs) ++ env
    uniq x y = fst x == fst y

startSpec :: Env -> Logger -> Spec -> IO [Async ExitCode]
startSpec env logger (name, procs) = do
    putStrLn $ "Starting " <> T.unpack name <> " processes ..."
    mapM (startProc env logger) procs

startProc :: Env -> Logger -> Proc -> IO (Async ExitCode)
startProc env logger Proc{..} = do
    putStrLn . init $ unlines
        [ "Forking " <> T.unpack procName <> " ..."
        , " Cmd: " <> cmd
        , " Dir: " <> procDir
        , " Env: " <> show env'
        ]
    async $ fst <$> shell cmd (Just procDir) env' logger
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
