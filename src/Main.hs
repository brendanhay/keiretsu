module Main (main) where

import Control.Applicative
import Data.Version              (showVersion)
import Keiretsu.Command
import System.Console.CmdTheLine
import System.Environment
import System.IO

import qualified Paths_keiretsu as P

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    name <- getProgName
    runChoice (defTerm name) [startTerm, cleanTerm]

defTerm :: String -> (Term (IO ()), TermInfo)
defTerm name = (term, info)
  where
    term = ret $ (\_ _ -> helpFail Pager Nothing) <$> tmp <*> config
    info = (describe
        "Keiretsu is an orchestration manager for integration\
        \based testing.  It allows you to specify dependencies\
        \that should be available before the start of a test run,\
        \triggers setup/teardown hooks, and applies a consistent\
        \environment to all child processes ensuring they can\
        \discover each other.")
        { version  = showVersion P.version
        , termName = name
        }

startTerm :: (Term (IO ()), TermInfo)
startTerm = (term, info)
  where
    term = start <$> config <*> tmp <*> env <*> verify <*> build <*> conc
    info = (describe
        "Rotate input gathered from INPUT or standard-in N \
        \places.  The input must be composed totally of \
        \alphabetic characters and spaces.")
        { termName = "start"
        , termDoc  = "Update, build, and start dependencies."
        }

    env = value . optAll ["./.env"] $ (optInfo ["env"])
        { optDoc = "Foreman style .env files to merge into all individual \
                   \processes environment.  Can be repeatedly specified."
        }

    verify = noFlag ["no-verify"]
        "Skip verification phase."

    build = noFlag ["no-build"]
        "Skip build phase."

    conc = noFlag ["no-concurrency"]
        "Don't fork workers for concurrent tasks."

cleanTerm :: (Term (IO ()), TermInfo)
cleanTerm = (clean <$> config <*> tmp <*> force, info)
  where
    info = (describe
        "Rotate input gathered from INPUT or standard-in N \
        \places.  The input must be composed totally of \
        \alphabetic characters and spaces.")
        { termName = "clean"
        , termDoc  = "Clean dependencies."
        }

    force = value . flag $ (optInfo ["force"])
        { optDoc = "Force removal of all vendored dependencies."
        }

common :: String
common = "COMMON OPTIONS"

describe :: String -> TermInfo
describe desc = defTI
    { stdOptSec = common
    , man =
        [ S "DESCRIPTION"
        , P desc
        , S common
        , S "MORE HELP"
        , P "Use '$(mname) $(i,COMMAND) --help' for help on a single command."
        ]
    }

tmp :: Term FilePath
tmp = value . opt "./tmp/integration" $ (optInfo ["tmp"])
    { optDoc = "Working directory for vendored dependencies."
    , optSec = common
    }

config :: Term FilePath
config = value . opt "./Intfile" $ (optInfo ["config"])
    { optDoc = "Configuration file containing dependency specifications."
    , optSec = common
    }

noFlag :: [String] -> String -> Term Bool
noFlag opts doc = value $ vFlag True [(False, (optInfo opts) { optDoc = doc })]
