module Keiretsu.Process (
      forkProcesses
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString          (ByteString, hGetSome)
import Data.Monoid
import Keiretsu.Config
import Keiretsu.Types
import System.Console.Rainbow
import System.Exit
import System.IO
import System.IO.Streams        (InputStream, OutputStream)
import System.Posix.Signals
import System.Process           (CreateProcess, ProcessHandle)
import System.Process.Internals (ProcessHandle__(..), withProcessHandle_)

import qualified Data.ByteString.Char8        as BS
import qualified Data.Text.Encoding           as E
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

forkProcesses :: Chan Signal -> Env -> [Proc] -> IO [Async ExitCode]
forkProcesses chan env ps = do
    putStrLn "Forking Processes ..."
    runCommands chan $ map (`makeCmd` e) ps
  where
    e = mergeEnvironment env ps

runCommands :: Chan Signal -> [Cmd] -> IO [Async ExitCode]
runCommands chan cmds = do
    (ps, ss) <- unzip <$> zipWithM runCommand (cycle colors) cmds
    supplyTerm ss >>= link
    waitForSignals chan ps
    mapM (waitForProcess chan) ps

runCommand :: ForegroundAll -> Cmd -> IO (ProcessHandle, InputStream [Chunk])
runCommand fmt cmd = do
    (Nothing, Just out, Nothing, pid) <- P.createProcess $ processSettings cmd
    i <- handleToInput fmt out $ cmdPre cmd
    return (pid, i)

waitForProcess :: Chan Signal -> ProcessHandle -> IO (Async ExitCode)
waitForProcess chan pid =
    async $ P.waitForProcess pid <* writeChan chan sigINT

waitForSignals :: Chan Signal -> [ProcessHandle] -> IO (Async ())
waitForSignals chan ps =
    async . forever $ readChan chan >>= \sig -> mapM (signalProcess' sig) ps

signalProcess' :: Signal -> ProcessHandle -> IO ()
signalProcess' sig pid =
    withProcessHandle_ pid $ \p -> case p of
        (ClosedHandle _) -> return p
        (OpenHandle hd)  -> signalProcess sig hd >> return p

processSettings :: Cmd -> CreateProcess
processSettings Cmd{..} =
    (P.shell . BS.unpack $ appendRedirect cmdStr)
        { P.std_out = P.CreatePipe
        , P.env     = if null cmdEnv then Nothing else Just cmdEnv
        , P.cwd     = cmdDir
        }

appendRedirect :: ByteString -> ByteString
appendRedirect bs = if suf `BS.isInfixOf` bs then bs else bs <> suf
  where
    suf = " 2>&1"

handleToInput :: ForegroundAll
              -> Handle
              -> ByteString
              -> IO (InputStream [Chunk])
handleToInput fmt hd pre = S.makeInputStream (readBuffer hd >>= f)
    >>= S.atEndOfInput (hClose hd)
    >>= S.lockingInputStream
  where
    f bs | BS.null bs         = return Nothing
         | BS.last bs == '\n' = return $! Just $ formatLines fmt pre bs
         | otherwise          = readBuffer hd >>= f . (bs <>)

readBuffer :: Handle -> IO ByteString
readBuffer = (`hGetSome` bufferSize)

formatLines :: ForegroundAll -> ByteString -> ByteString -> [Chunk]
formatLines fmt pre = concatMap f . BS.lines
  where
    f bs = [ plain (E.decodeUtf8 pre <> ": ") +.+ fmt +.+ bold
           , plain (E.decodeUtf8 bs) +.+ fmt
           , plain "\n"
           ]

termToOutput :: Term -> IO (OutputStream [Chunk])
termToOutput term = S.makeOutputStream f >>= S.lockingOutputStream
  where
    f Nothing   = return ()
    f (Just cs) = printChunks term cs

supplyTerm :: [InputStream [Chunk]] -> IO (Async ())
supplyTerm ss =
    async . join $ S.supply
        <$> S.concurrentMerge ss
        <*> (termFromEnv >>= termToOutput)
