{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Keiretsu.Orphans
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Keiretsu.Orphans where

import System.Process
import System.Process.Internals

instance Eq ProcessHandle where
    (ProcessHandle a _) == (ProcessHandle b _) = a == b

instance Show ProcessHandle where
    show _ = "ProcessHandle"
