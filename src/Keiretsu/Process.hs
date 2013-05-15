module Keiretsu.Process (
      fromDependencies
    , start
    ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.ByteString          (ByteString)
import Data.Char
import Data.Function
import Data.List
import Data.Monoid
import Data.Word
import Network.Socket
import Keiretsu.Environment
import Keiretsu.Shell
import Keiretsu.Types
import System.FilePath
import System.Exit

import qualified Data.ByteString.Char8 as BS
import qualified Keiretsu.Config       as Cfg
import qualified System.Process        as P

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
    putStrLn "Setting up streams ..."
    ps <- mergeCommands $ map (`makeCommand` env') procs
    mapM (async . P.waitForProcess) ps
  where
    procs = concatMap snd specs
    env'  = nubBy ((==) `on` fst) $ fromProcs procs ++ env

makeCommand :: Proc -> Env -> Cmd
makeCommand Proc{..} env = Cmd
    ((snd . BS.breakEnd (== '/') . BS.pack $ procDir) <> "/" <> procName)
    procCmd
    (Just procDir)
    (("PORT", show procPort) : env)

portVar :: ByteString -> ByteString -> ByteString
portVar x y = BS.map toUpper $ BS.intercalate "_" [x, y, "port"]

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
