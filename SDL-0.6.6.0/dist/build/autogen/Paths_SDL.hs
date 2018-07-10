{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_SDL (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,6,6,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/x86_64-linux-ghc-7.10.3/SDL-0.6.6.0-22k7wGXfyMM8B0lwRm2Uis"
dynlibdir  = "/usr/local/lib/x86_64-linux-ghc-7.10.3"
datadir    = "/usr/local/share/x86_64-linux-ghc-7.10.3/SDL-0.6.6.0"
libexecdir = "/usr/local/libexec/x86_64-linux-ghc-7.10.3/SDL-0.6.6.0"
sysconfdir = "/usr/local/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SDL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SDL_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SDL_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SDL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SDL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SDL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
