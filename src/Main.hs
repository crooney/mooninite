import           System.Console.GetOpt
import           System.Environment
import           System.Process
import           System.Exit
import           System.IO
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Version

import           Parser
import           Runtime
import qualified Paths_mooninite as P

-- oion stuff all based on GetOpt haddock example
data Options   = Options { oVerbose        :: Bool
                         , oShowVersion    :: Bool
                         , oCheckSyntax    :: Bool
                         , oIntermediate   :: Bool
                         , oOptimize       :: Bool
                         , oIncludeRuntime :: Bool
                         , oOutput         :: FilePath
                         , oStdOut         :: String
                         , oLua            :: String
                         }
                         deriving Show

defaultOptions = Options { oVerbose        = False
                         , oShowVersion    = False
                         , oCheckSyntax    = False
                         , oIntermediate   = False
                         , oOptimize       = False
                         , oIncludeRuntime = False
                         , oOutput         = "-"
                         , oStdOut         = ""
                         , oLua            = "lua"
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "v"     ["verbose"]
    (NoArg (\os -> os { oVerbose = True }))
    "chatty output on stderr"
    , Option "V?"    ["version"]
    (NoArg (\os -> os { oStdOut = version}))
    "show version number"
    , Option "O"     ["oimize"]
    (NoArg (\os -> os { oOptimize = True }))
    "oimize (currently does nothing)"
    , Option "s"     ["check-syntax"]
    (NoArg (\os -> os { oCheckSyntax = True }))
    "check syntax and exit"
    , Option "c"     ["intermediate"]
    (NoArg (\os -> os { oIntermediate = True }))
    "output lua file and exit without invoking lua interpreter"
    , Option "P"     ["print-runtime"]
    (NoArg (\os -> os { oStdOut = runtime}))
    "print runtime to standard output and exit"
    , Option "I"     ["include-runtime"]
    (NoArg (\os -> os { oIncludeRuntime = True }))
    "include runtime directly in output instead of in library"
    , Option "h"     ["help"]
    (NoArg (\os -> os { oStdOut = usage}))
    "show this message and exit"
    , Option "i"     ["interpreter"]
    (ReqArg (\f os -> os { oLua = f }) "lua")
    "lua complier/interpreter to invoke; defaults to 'lua'"
    , Option "o"     ["output"]
    (ReqArg (\f os -> os { oOutput = f }) "-")
    "output FILE; defaults to stdout (-)"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError $ userError $ concat errs ++ usage

usage,version :: String
usage = usageInfo header options
  where header = version ++ "\nUsage: mooninite [OPTION...] files...\n"
                         ++ "       use - to read from stdin"
version = "mooninite version " ++ showVersion P.version ++ "\n"

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
    (os,as) <- getArgs >>= compilerOpts
    info os $ "compiling to " ++ oOutput os
    liftM (compile as) (allIn as) >>= allOut (oOutput os)
    when (oIntermediate os || oOutput os == "-") exitSuccess
    (code,out,err) <- readProcessWithExitCode (oLua os) [oOutput os] ""
    when (code /= ExitSuccess) $ info os $ oLua os ++ " failed with: "
    putStr out >> hPutStr stderr err
    exitWith code
    return ()
  where compile as = generateLua . parseLisp (intercalate ", " as)
        info os m = when (oVerbose os) $ hPutStrLn stderr m
