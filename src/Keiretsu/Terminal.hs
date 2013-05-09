module Keiretsu.Terminal (
      shell
    , getLogger
    ) where

import Control.Concurrent
import Control.Monad
import Control.Exception  (evaluate)
import Data.IORef
import Keiretsu.Types
import System.Exit
import System.IO

import qualified System.Process as P

shell :: String
      -> Maybe FilePath
      -> Env
      -> Logger
      -> IO (ExitCode, String)
shell sh cwd env logger = do
    (Nothing, Just hOut, Just hErr, hProc) <- P.createProcess $ (P.shell sh)
        { P.std_out = P.CreatePipe
        , P.std_err = P.CreatePipe
        , P.cwd     = cwd
        , P.env     = Just env
        }

    out <- hGetContents hOut
    _   <- evaluate $ length out
    logger out

    err <- hGetContents hErr
    _   <- evaluate $ length err
    logger err

    hClose hOut
    hClose hErr

    ec  <- P.waitForProcess hProc

    return (ec, out)

getLogger :: IO Logger
getLogger = return putStrLn
