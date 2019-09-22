module Main (main) where

import Data.Maybe (fromJust)
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
    ( confHook
    , defaultMainWithHooks
    , simpleUserHooks
    )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , configFlags
    , localPkgDescr
    )
import Distribution.Simple.Setup (ConfigFlags)
import System.Directory (getCurrentDirectory)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    confHook = myConfHook
}

myConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
myConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ PD.library packageDescription
        libraryBuildInfo = PD.libBuildInfo library
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            PD.library = Just $ library {
                PD.libBuildInfo = libraryBuildInfo {
                    PD.extraLibDirs = (dir ++ "/.vs-work"):PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }
