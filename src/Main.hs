module Main (main) where

import Control.Applicative
import Data.Version              (showVersion)
import Keiretsu.Command
import System.Console.CmdTheLine
import System.Environment

import qualified Paths_keiretsu as P

main :: IO ()
main = do
    name <- getProgName
    runChoice (defTerm name)
        [ cleanTerm "clean"
        , startTerm "start"
        , integrateTerm "integrate"
        ]

defTerm :: String -> (Term (IO ()), TermInfo)
defTerm name = (term, info)
  where
    term = ret $ (\_ _ -> helpFail Pager Nothing)
        <$> tmp
        <*> config

    info = (describe
        "Keiretsu is an orchestration manager for integration \
        \based testing.  It allows you to specify dependencies \
        \that should be available before the start of a test run,\
        \triggers setup/teardown hooks, and applies a consistent \
        \environment to all child processes ensuring they can \
        \discover each other.")
        { version  = showVersion P.version
        , termName = name
        }

cleanTerm :: String -> (Term (IO ()), TermInfo)
cleanTerm name = (term, info)
  where
    term = clean
        <$> config
        <*> tmp
        <*> force

    info = (describe
        "Run `make clean` for each vendored dependency, or using the --force \
        \flag the entire integration workspace will be removed.")
        { termName = name
        , termDoc  = "Clean dependencies."
        }

startTerm :: String -> (Term (IO ()), TermInfo)
startTerm name = (term, info)
  where
    term = start
        <$> envs
        <*> dump

    info = (describe
        "Parses and runs the proctypes from a Procfile in the current \
        \working directory. Equivalent to running: `foreman start`")
        { termName = name
        , termDoc  = "Start the local Procfile."
        }

integrateTerm :: String -> (Term (IO ()), TermInfo)
integrateTerm name = (term, info)
  where
    term = integrate
        <$> config
        <*> tmp
        <*> envs
        <*> runs
        <*> excludes
        <*> delay
        <*> dump
        <*> verify
        <*> build

    info = (describe
        "Retrieve (or update) dependencies, build them using their \
        \respective Makefiles, and then start all the dependencies \
        \according to their respective Procfiles. \
        \stdout and stderr from the running processes is interleaved \
        \into the parent's stdout and signals from the parent are \
        \trapped and delivered to the children. \
        \Any errors in the children are propagated to the parent and \
        \cause the sibling processes to exit.")
        { termName = name
        , termDoc  = "Update, build, and start all dependencies."
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

envs :: Term [FilePath]
envs = value . optAll [] $ (optInfo ["env"])
    { optDoc = "Foreman style .env files to merge into each processes' \
               \environment.  Can be repeatedly specified.  If a .env file \
               \exists in the working directory, it will be loaded and \
               \merged with a lower precedence."
    }

excludes :: Term [String]
excludes = value . optAll [] $ (optInfo ["exclude"])
    { optDoc = "Implies --delay"
    }

runs :: Term [String]
runs = value . optAll [] $ (optInfo ["run"])
    { optDoc = "Implies --delay"
    }

force :: Term Bool
force = value . flag $ (optInfo ["force"])
    { optDoc = "Force removal of the integration workspace."
    }

dump :: Term Bool
dump = value . flag $ (optInfo ["dump"])
    { optDoc = "Dump the environment for each process to stdout prior to starting."
    }

delay :: Term Int
delay = value . opt 1 $ (optInfo ["delay"])
    { optDoc = "Delay after dependency start, before forking --run arguments"
    }

verify :: Term Bool
verify = noFlag ["no-verify"] "Skip verification phase."

build :: Term Bool
build = noFlag ["no-build"] "Skip build phase."

noFlag :: [String] -> String -> Term Bool
noFlag opts doc = value $ vFlag True [(False, (optInfo opts) { optDoc = doc })]
