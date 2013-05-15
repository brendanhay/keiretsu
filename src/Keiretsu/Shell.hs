module Keiretsu.Shell (
      Cmd(..)
    , mergeCommands
    , runCommand
    , formatLines
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString        (ByteString, hGetSome)
import Data.Monoid
import Keiretsu.Types
import System.Console.Rainbow
import System.IO
import System.IO.Streams      (InputStream, OutputStream)
import System.Process         (CreateProcess, ProcessHandle)

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

mergeCommands :: [Cmd] -> IO [ProcessHandle]
mergeCommands cmds = do
    term     <- termFromEnv
    (ss, ps) <- unzip <$> zipWithM runCommand (cycle colors) cmds
    inp      <- S.concurrentMerge ss
    out      <- createOutput term
    S.supply inp out
    return ps

runCommand :: ForegroundAll -> Cmd -> IO (InputStream [Chunk], ProcessHandle)
runCommand fmt cmd = do
    (Nothing, Just hOut, Nothing, pd) <- P.createProcess $ processSettings cmd
    hd <- createInput fmt hOut $ cmdPre cmd
    return (hd, pd)

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
