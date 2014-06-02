import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Data.Char (toUpper,toLower)
import Control.Monad (liftM)
import System.FilePath (takeBaseName)

main = defaultMainWithHooks theHooks
  where theHooks = simpleUserHooks
          {hookedPreProcessors = ("lua",ppTextAsString):knownSuffixHandlers}

ppTextAsString :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppTextAsString _ _ = PreProcessor
                       {platformIndependent = True,
                        runPreProcessor = mkSimplePreProcessor pp}
  where
    pp i o v = info v ("converting " ++ i ++ " to " ++ o)
               >> liftM process (readFile i) >>= writeFile o
      where
        process = (header ++) . unlines . map go . lines
        go x = "  ++ \"" ++ x ++ "\\n\""
        header = "module "++uname ++ " where\n\n" ++ dname ++ " :: String\n"
                 ++ dname ++ " = \"\"\n"
        dname = toLower (head $ takeBaseName i) : tail (takeBaseName i)
        uname = toUpper (head dname) : tail dname
