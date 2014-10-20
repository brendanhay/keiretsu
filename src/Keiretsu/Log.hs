{-# LANGUAGE OverloadedStrings #-}

-- Module      : Keiretsu.Log
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Keiretsu.Log where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as Text
import           System.Console.ANSI
import           System.IO
import           System.Log.Handler.Simple
import           System.Log.Logger

logName :: String
logName = "log"

logDebugBS :: ByteString -> IO ()
logDebugBS = logDebug . BS.unpack

logError, logDebug :: String -> IO ()
logError = logMsg errorM
logDebug = logMsg debugM

logMsg :: (String -> a -> IO ()) -> a -> IO ()
logMsg f = f logName

setLogging :: Bool -> IO ()
setLogging debug = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    removeAllHandlers
    hd <- streamHandler stderr prio
    updateGlobalLogger logName (setLevel prio . setHandlers [hd])
  where
    prio = if debug then DEBUG else INFO

colours :: [Color]
colours = cycle [Red, Green, Blue, Magenta, Yellow, Cyan]

colourise :: Color -> Text -> ByteString -> ByteString
colourise c x y = prefix
    <> Text.encodeUtf8 x
    <> suffix
    <> ": "
    <> y
    <> clear
  where
    prefix = BS.pack $ setSGRCode
        [ SetColor Foreground Vivid c
        , SetConsoleIntensity BoldIntensity
        ]

    suffix = BS.pack $ setSGRCode
        [ SetColor Foreground Vivid c
        , SetConsoleIntensity NormalIntensity
        ]

    clear = BS.pack $ setSGRCode []

