#!/usr/bin/env runhaskell

TODO: update the po/kata.pot and any procedural .po files here?

\begin{code}
{-# LANGUAGE CPP #-}

import Control.Monad ( zipWithM_, when, unless, filterM )
import Data.Version( showVersion )
import Distribution.Text ( display )
import Distribution.System (OS(Windows), buildOS)
import Distribution.Version (Version(versionBranch))
import Distribution.Package ( packageVersion, packageName, PackageName(..) )
import Distribution.PackageDescription
         ( PackageDescription(executables), Executable(buildInfo, exeName)
         , BuildInfo(customFieldsBI), emptyBuildInfo
         , updatePackageDescription, cppOptions, ccOptions
         , library, libBuildInfo, otherModules )
import Distribution.Simple (defaultMainWithHooks, UserHooks(..), simpleUserHooks) 
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), absoluteInstallDirs, externalPackageDeps )
import Distribution.Simple.Setup (buildVerbosity, copyDest, copyVerbosity, fromFlag, haddockVerbosity, installVerbosity, sDistVerbosity)
import Distribution.Simple.Utils (copyFiles, createDirectoryIfMissingVerbose, rawSystemStdout, rewriteFile)
import Distribution.Verbosity ( Verbosity )
import System.Directory
    (copyFile, createDirectory, createDirectoryIfMissing,
     doesDirectoryExist, doesFileExist,
     getCurrentDirectory, getDirectoryContents,
     removeDirectoryRecursive, removeFile, setCurrentDirectory)
import System.IO (openFile, IOMode (..))
import System.FilePath ( (</>), splitDirectories, isAbsolute )

main = defaultMainWithHooks simpleUserHooks 
    { buildHook = kataBuildHook
    , haddockHook = kataHaddockHook }

kataBuildHook pkg lbi hooks flags = do
    x <- commonBuildHook buildHook pkg lbi hooks (fromFlag $ buildVerbosity flags)
    x flags 
        
kataHaddockHook pkg lbi hooks flags = do
    x <- commonBuildHook haddockHook pkg lbi hooks (fromFlag $ haddockVerbosity flags)
    x flags

commonBuildHook :: (UserHooks -> PackageDescription -> LocalBuildInfo -> t -> a) -> PackageDescription -> LocalBuildInfo -> t -> Verbosity -> IO a
commonBuildHook runHook pkg lbi hooks verbosity = do
  let version = display (packageVersion pkg) 
  generateVersionModule verbosity pkg lbi version 
  return $ runHook simpleUserHooks pkg lbi hooks 

-- TODO: pass actual objects rather than strings here
generateVersionModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> String -> IO ()
generateVersionModule verbosity pkg lbi version = do
    createDirectoryIfMissingVerbose verbosity True dir
    rewriteFile (dir </> "Version.hs") $ unlines
        ["module Version (builddeps, version) where"
        ,"builddeps, version :: String"
        ,"version = \"" ++ version ++ "\""
        ,"builddeps = " ++ (show $ formatdeps $ externalPackageDeps lbi)
        ]
  where formatdeps = unlines . map (formatone . snd)
        formatone p = '\t' : n ++ "-" ++ showVersion (packageVersion p)
            where PackageName n = packageName p
        dir = autogenModulesDir lbi


\end{code}
