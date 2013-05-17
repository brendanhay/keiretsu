module Keiretsu.Process (
      runCommands
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString          (ByteString, hGetSome)
import Data.List
import Data.Monoid
import Data.Text                (Text)
import Keiretsu.Types
import System.Console.Rainbow
import System.Exit
import System.IO
import System.IO.Streams        (InputStream, OutputStream)
import System.Posix.Signals
import System.Process           (CreateProcess, ProcessHandle)
import System.Process.Internals (ProcessHandle__(..), withProcessHandle_)

import qualified Data.ByteString.Char8        as BS
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import qualified System.IO.Streams            as S
import qualified System.IO.Streams.Concurrent as S
import qualified System.Process               as P

bufferSize :: Int
bufferSize = 32752

runCommands :: Bool -> SignalChan -> [Cmd] -> IO [Async ExitCode]
runCommands dump chan cmds = do
    (ps, ss) <- unzip <$> mapM runCommand cmds
    term     <- termFromEnv

    supplyTerm term ss >>= link

    when dump $ dumpEnv term cmds

    waitForSignals chan ps
    mapM (waitForProcess chan) ps

runCommand :: Cmd -> IO (ProcessHandle, InputStream [Chunk])
runCommand cmd = do
    (Nothing, Just out, Nothing, pid) <- P.createProcess $ processSettings cmd
    i <- handleToInput cmd out
    return (pid, i)

waitForProcess :: SignalChan -> ProcessHandle -> IO (Async ExitCode)
waitForProcess chan pid =
    async $ P.waitForProcess pid <* writeChan chan (sigINT, Just pid)

waitForSignals :: SignalChan -> [ProcessHandle] -> IO (Async ())
waitForSignals chan ps = async . forever $ do
    (sig, pid) <- readChan chan
    mapM (signalProcess' sig) $ filter ((pid /=) . Just) ps

signalProcess' :: Signal -> ProcessHandle -> IO ()
signalProcess' sig pid =
    withProcessHandle_ pid $ \p -> case p of
        (ClosedHandle _) -> return p
        (OpenHandle hd)  -> signalProcess sig hd >> return p

processSettings :: Cmd -> CreateProcess
processSettings Cmd{..} =
    (P.shell $ appendRedirect cmdStr)
        { P.std_out = P.CreatePipe
        , P.env     = if null cmdEnv then Nothing else Just cmdEnv
        , P.cwd     = cmdDir
        }

appendRedirect :: String -> String
appendRedirect str = if suf `isInfixOf` str then str else str <> suf
  where
    suf = " 2>&1"

handleToInput :: Cmd -> Handle -> IO (InputStream [Chunk])
handleToInput cmd hd = S.makeInputStream (readBuffer hd >>= f)
    >>= S.atEndOfInput (hClose hd)
    >>= S.lockingInputStream
  where
    f bs | BS.null bs         = return Nothing
         | BS.last bs == '\n' = return $! Just $ formatLines cmd bs
         | otherwise          = readBuffer hd >>= f . (bs <>)

readBuffer :: Handle -> IO ByteString
readBuffer = (`hGetSome` bufferSize)

dumpEnv :: Term -> [Cmd] -> IO ()
dumpEnv term = mapM_ (printChunks term . formatEnv)

formatEnv :: Cmd -> [Chunk]
formatEnv cmd = formatLine cmd ("Environment: " <> T.pack (cmdStr cmd))
    ++ concatMap f (cmdEnv cmd)
  where
    f (k, v) = formatLine cmd $ T.pack k <> ": " <> T.pack v

formatLines :: Cmd -> ByteString -> [Chunk]
formatLines cmd =
    concatMap (formatLine cmd . E.decodeUtf8) . BS.lines

formatLine :: Cmd -> Text -> [Chunk]
formatLine Cmd{..} txt =
    [ plain (T.pack cmdPre <> ": ") +.+ cmdColor +.+ bold
    , plain (txt <> "\n") +.+ cmdColor
    ]

supplyTerm :: Term -> [InputStream [Chunk]] -> IO (Async ())
supplyTerm term ss =
    async . join $ S.supply
        <$> S.concurrentMerge ss
        <*> termToOutput term

termToOutput :: Term -> IO (OutputStream [Chunk])
termToOutput term = S.makeOutputStream f >>= S.lockingOutputStream
  where
    f Nothing   = return ()
    f (Just cs) = printChunks term cs

