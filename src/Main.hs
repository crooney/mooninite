import           System.Console.GetOpt
import           System.Process
import           System.Exit
import           System.IO
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Version
import           Options.Applicative

import           Parser (parseLisp)
import           Runtime
import qualified Paths_mooninite as P

data Options = Options { oVerbose        :: Bool
                       , oIntermediate   :: Bool
                       , oLua            :: String
                       , oOutput         :: FilePath
                       , oInput          :: [FilePath]
                       }

options :: Parser Options
options = Options
    <$> switch ( short 'v'      <> help "Verbose output to STDERR" )
    <*> switch ( short 'c'
       <> help "Compile to lua but don't invoke lua interpreter" )
    <*> strOption ( short 'i' <> metavar "PROGRAM" <> value "lua" <> showDefault
       <> help "Invoke PROGRAM as lua interpreter" )
    <*> strOption ( short 'o' <> metavar "FILE"
       <> help "Output to FILE instead of STDOUT" )
    <* infoOption version ( long "version" <> help "Display version info" )
    <* infoOption runtime ( long "print-runtime"
       <> help "Print lua runtime to STDOUT" )
    <*> some (argument str (metavar "FILES..."))

version :: String
version = "mooninite version " ++ showVersion P.version

-- Once we start reading from stdin, that's it.
allIn :: [FilePath] -> IO String
allIn [] = return ""
allIn ("-":_) = getContents
allIn (x:xs) = (++) <$> readFile x <*> allIn xs

main :: IO ()
main = execParser opts >>= mainWithOpts
  where opts = info (helper <*> options)
                ( fullDesc
                  <> progDesc "Compile FILES to lua. Use '-' for STDIN."
                  <> header "mooninite - lisp -> lua" )

mainWithOpts :: Options -> IO ()
mainWithOpts os = do
    inform $ "Compiling to " ++ if null (oOutput os) then "STDOUT"
                                                     else oOutput os
    liftM (compile $ oInput os) (allIn $ oInput os) >>= fOut
    when (oIntermediate os || null (oOutput os)) exitSuccess
    (code,out,err) <- readProcessWithExitCode (oLua os) [oOutput os] ""
    when (code /= ExitSuccess) $ inform $ oLua os ++ " failed with: "
    putStr out >> hPutStr stderr err
    _ <- exitWith code
    return ()
  where compile as = parseLisp (intercalate ", " as)
        inform m = when (oVerbose os) $ hPutStrLn stderr m
        fOut = if null (oOutput os) then putStr else writeFile (oOutput os)
