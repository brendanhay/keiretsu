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

-- getLogger :: IO Logger
-- getLogger = do
--     chan   <- newChan
--     isTerm <- hIsTerminalDevice stdout
--     osRef  <- newIORef []
--     lock   <- newQSem 1

--     return $ \s -> unless (null s) $ do
--         waitQSem lock
--         t  <- myThreadId
--         os <- readIORef osRef

--         let all_lines = lines $ maybe s (++s) (lookup t os)
--         let done = last s == '\n'

--         let (done_lines, current_line) = if done || null all_lines
--                                           then (all_lines,   Nothing)
--                                           else (init all_lines, Just (last all_lines))

--         when isTerm $ do
--             unless (null os) $ do
--                 -- Go up some lines
--                 putStr $ "\ESC[" ++ show (length os) ++ "A"
--                 -- Clear everything
--                 unless (null os) $ putStr "\ESC[0J"
--             -- Go up once more, to make it fit
--             putStr "\ESC[1A"

--             -- Write a new done line, if suitable
--             mapM_ putLnStr done_lines

--         unless isTerm $ mapM_ putStrLn done_lines

--         -- Update line
--         let os'= replaceAppendOrDelete t current_line os

--         when isTerm $ do
--             -- Write the new not-yet-done lines
--             mapM_ (putLnStr . snd) os'

--             -- Go to the beginning of the next new line
--             putStrLn ""

--         writeIORef osRef os'
--         signalQSem lock

-- replaceAppendOrDelete :: (Eq a) => a -> Maybe b -> [(a, b)] -> [(a, b)]
-- replaceAppendOrDelete a Nothing  []                     = []
-- replaceAppendOrDelete a (Just b) []                     = [(a,b)]
-- replaceAppendOrDelete a Nothing  ((a',_):xs)  | a == a' = xs
-- replaceAppendOrDelete a (Just b) ((a',b'):xs) | a == a' = (a,b):xs
-- replaceAppendOrDelete a mbB      (x:xs)                 = x : replaceAppendOrDelete a mbB xs

-- putLnStr :: String -> IO ()
-- putLnStr s = putStr ('\n':s)
