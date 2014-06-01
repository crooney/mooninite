import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Version

import           Parser
import qualified Paths_mooninite as P

-- option stuff all based on GetOpt haddock example
data Options   = Options { optVerbose        :: Bool
                         , optShowVersion    :: Bool
                         , optCheckSyntax    :: Bool
                         , optIntermediate   :: Bool
                         , optOptimize       :: Bool
                         , optIncludeRuntime :: Bool
                         , optOutput         :: FilePath
                         , optStdOut         :: String
                         , optStdErr         :: String
                         }
                         deriving Show

defaultOptions = Options { optVerbose        = False
                         , optShowVersion    = False
                         , optCheckSyntax    = False
                         , optIntermediate   = False
                         , optOptimize       = False
                         , optIncludeRuntime = False
                         , optOutput         = "-"
                         , optStdOut         = ""
                         , optStdErr         = ""
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "v"     ["verbose"]
    (NoArg (\opts -> opts { optVerbose = True }))
    "chatty output on stderr"
    , Option "V?"    ["version"]
    (NoArg (\opts -> opts { optStdOut = version}))
    "show version number"
    , Option "O"     ["optimize"]
    (NoArg (\opts -> opts { optOptimize = True }))
    "optimize (currently does nothing)"
    , Option "s"     ["check-syntax"]
    (NoArg (\opts -> opts { optCheckSyntax = True }))
    "check syntax and exit"
    , Option "c"     ["intermediate"]
    (NoArg (\opts -> opts { optIntermediate = True }))
    "output lua file and exit without invoking lua intepreter"
    , Option "P"     ["print-runtime"]
    (NoArg (\opts -> opts { optStdOut = runtime}))
    "print runtime to standard output and exit"
    , Option "I"     ["include-runtime"]
    (NoArg (\opts -> opts { optIncludeRuntime = True }))
    "include runtime directly in output instead of in library"
    , Option "h"     ["help"]
    (NoArg (\opts -> opts { optStdOut = usage}))
    "show help"
    , Option "o"     ["output"]
    (ReqArg (\f opts -> opts { optOutput = f }) "-")
    "output FILE; defaults to stdout (-)"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError $ userError $ concat errs ++ usage

usage,version,runtime :: String
usage = usageInfo header options
  where header = version ++ "\nUsage: mooninite [OPTION...] files...\n"
                         ++ "       use - to read from stdin"

version = "mooninite version " ++ (showVersion P.version) ++ "\n"
runtime = "runtime\n"

-- Once we start reading from stdin, that's it.
allIn :: [FilePath] -> IO String
allIn [] = return ""
allIn ("-":_) = getContents
allIn (x:xs) = (++) <$> readFile x <*> allIn xs

allOut :: FilePath -> String -> IO ()
allOut "-" = putStr
allOut p   = writeFile extended
    where extended = if '.' `elem` p then p else p ++ ".lua"

generateLua :: Show a => a -> String
generateLua = show

main = do
    x <- getArgs
    (os,as) <- compilerOpts x
    when (optStdOut os /= "") $ putStr (optStdOut os) >> exitSuccess
    lisp <- allIn as
    let lua = generateLua $ parseLisp (intercalate ", " as) lisp
    allOut (optOutput os) lua
    return ()
