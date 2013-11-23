module Paths_BondAngleDihedral (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/alessio/.cabal/bin"
libdir     = "/home/alessio/.cabal/lib/BondAngleDihedral-0.1.0.0/ghc-7.6.3"
datadir    = "/home/alessio/.cabal/share/BondAngleDihedral-0.1.0.0"
libexecdir = "/home/alessio/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "BondAngleDihedral_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "BondAngleDihedral_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "BondAngleDihedral_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "BondAngleDihedral_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
