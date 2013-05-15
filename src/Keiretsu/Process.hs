module Keiretsu.Process (
      load
    , start
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString          (ByteString, hGetSome)
import Data.Monoid
import Data.Word
import Keiretsu.Types
import Network.Socket
import System.Console.Rainbow
import System.Exit
import System.FilePath
import System.IO
import System.IO.Streams        (InputStream, OutputStream)
import System.Posix.Signals
import System.Process           (CreateProcess, ProcessHandle)
import System.Process.Internals (ProcessHandle__(..), withProcessHandle_)

import qualified Data.ByteString.Char8        as BS
import qualified Data.Text.Encoding           as E
import qualified Keiretsu.Config              as Cfg
import qualified Keiretsu.Environment         as Env
import qualified System.IO.Streams            as S
import qualified System.IO.Streams.Concurrent as S
import qualified System.Process               as P

bufferSize :: Int
bufferSize = 32752

colors :: [ForegroundAll]
colors =
    [ f_red
    , f_green
    , f_yellow
    , f_blue
    , f_magenta
    , f_cyan
    , f_white
    ]

load :: [Dep] -> IO [Proc]
load = liftM concat . mapM processes

start :: Chan Signal -> Env -> [Proc] -> IO [Async ExitCode]
start chan env ps = do
    putStrLn "Setting up streams ..."
    runCommands chan $ map (`makeCmd` env') ps
  where
    env' = Env.merge env ps

processes :: Dep -> IO [Proc]
processes d = do
    putStrLn $ "Loading " <> cfg <> " ..."
    xs <- Cfg.load (makeProc d) cfg
    ys <- findPorts $ length xs
    return $ zipWith ($) xs ys
  where
    cfg = joinPath [depPath d, "Procfile"]

findPorts :: Int -> IO [Word16]
findPorts n = do
    ss <- sequence . take n $ repeat findSocket
    ps <- mapM ((fromIntegral <$>) . socketPort) ss
    mapM_ close ss
    return ps

findSocket :: IO Socket
findSocket = do
    s <- socket AF_INET Stream defaultProtocol
    a <- inet_addr "127.0.0.1"
    bind s $ SockAddrInet aNY_PORT a
    return s

runCommands :: Chan Signal -> [Cmd] -> IO [Async ExitCode]
runCommands chan cmds = do
    (ss, as) <- unzip <$> zipWithM (runOne chan) (cycle colors) cmds
    connectTerm ss
    return as

connectTerm :: [InputStream [Chunk]] -> IO ()
connectTerm ss = join $ S.supply
    <$> S.concurrentMerge ss
    <*> (termFromEnv >>= createOutput)

runOne :: Chan Signal
       -> ForegroundAll
       -> Cmd
       -> IO (InputStream [Chunk], Async ExitCode)
runOne chan fmt cmd = do
    (Nothing, Just hOut, Nothing, pid) <- P.createProcess $ processSettings cmd
    i <- createInput fmt hOut $ cmdPre cmd
    c <- dupChan chan
    a <- forkWait c pid
    return (i, a)

forkWait :: Chan Signal -> ProcessHandle -> IO (Async ExitCode)
forkWait chan pid = async $ do
    a <- async g
    b <- async $ P.waitForProcess pid <* cancel a
    wait b
  where
    g = withProcessHandle_ pid $ \p ->
        case p of
            (ClosedHandle _) -> return p
            (OpenHandle hd)  -> do
                int <- readChan chan
                signalProcess int hd
                return p

processSettings :: Cmd -> CreateProcess
processSettings Cmd{..} = (P.shell . BS.unpack $ appendRedirect cmdStr)
    { P.std_out = P.CreatePipe
    , P.env     = if null cmdEnv then Nothing else Just cmdEnv
    , P.cwd     = cmdDir
    }

appendRedirect :: ByteString -> ByteString
appendRedirect bs = if suf `BS.isInfixOf` bs then bs else bs <> suf
  where
    suf = " 2>&1"

createInput :: ForegroundAll -> Handle -> ByteString -> IO (InputStream [Chunk])
createInput fmt hd pre = S.makeInputStream (readBuffer hd >>= f)
    >>= S.atEndOfInput (hClose hd)
    >>= S.lockingInputStream
  where
    f bs | BS.null bs         = return Nothing
         | BS.last bs == '\n' = return $! Just $ formatLines fmt pre bs
         | otherwise          = readBuffer hd >>= f . (bs <>)

createOutput :: Term -> IO (OutputStream [Chunk])
createOutput term = S.makeOutputStream f >>= S.lockingOutputStream
  where
    f Nothing   = return ()
    f (Just cs) = printChunks term cs

readBuffer :: Handle -> IO ByteString
readBuffer = (`hGetSome` bufferSize)

formatLines :: ForegroundAll -> ByteString -> ByteString -> [Chunk]
formatLines fmt pre = concatMap f . BS.lines
  where
    f bs = [ plain (E.decodeUtf8 pre <> ": ") +.+ fmt +.+ bold
           , plain (E.decodeUtf8 bs) +.+ fmt
           , plain "\n"
           ]
