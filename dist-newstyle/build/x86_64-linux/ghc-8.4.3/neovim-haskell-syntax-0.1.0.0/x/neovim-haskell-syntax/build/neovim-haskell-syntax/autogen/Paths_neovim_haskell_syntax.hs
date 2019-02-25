{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_neovim_haskell_syntax (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/zachchurchill/.cabal/bin"
libdir     = "/home/zachchurchill/.cabal/lib/x86_64-linux-ghc-8.4.3/neovim-haskell-syntax-0.1.0.0-inplace-neovim-haskell-syntax"
dynlibdir  = "/home/zachchurchill/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/zachchurchill/.cabal/share/x86_64-linux-ghc-8.4.3/neovim-haskell-syntax-0.1.0.0"
libexecdir = "/home/zachchurchill/.cabal/libexec/x86_64-linux-ghc-8.4.3/neovim-haskell-syntax-0.1.0.0"
sysconfdir = "/home/zachchurchill/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "neovim_haskell_syntax_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "neovim_haskell_syntax_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "neovim_haskell_syntax_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "neovim_haskell_syntax_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "neovim_haskell_syntax_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "neovim_haskell_syntax_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
