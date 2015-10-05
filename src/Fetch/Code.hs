{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , ScopedTypeVariables
  #-}

module Fetch.Code where

import Fetch.Versions
import Fetch.Cabal

import Network.HTTP.Client             hiding (decompress)
import Network.HTTP.Client.TLS
import Codec.Compression.GZip
import Codec.Archive.Tar
import System.Directory
import Control.Concurrent.Async
import Language.Haskell.Exts           hiding (ModuleName)
import Distribution.PackageDescription
import Distribution.ModuleName         (ModuleName, toFilePath)

import qualified Data.Map              as Map
import qualified Data.ByteString.Lazy  as LBS
import Data.Maybe
import Data.Functor.Syntax


-- | Ping the Hackage server for a list of all packages
haskellModules :: String -- ^ Package name
               -> HackageVersion
               -> IO (Map.Map ModuleName Module)
haskellModules packageName vs = do
  description <- hackagePackageDescription packageName vs

  let packageString = packageName ++ "-" ++ showHackageVersion vs

  manager <- newManager tlsManagerSettings
  request <- parseUrl $ "https://hackage.haskell.org/package/"
                      ++ packageString
                      ++ "/" ++ packageString ++ ".tar.gz"

  unparsed' <- httpLbs request manager
  let unparsed = responseBody unparsed'
      tempDir = "tmp/" ++ packageString
      libData = fromMaybe (error $ "No library stanza in " ++ packageString)
                  $ (condTreeData <$> condLibrary description)
      hsSrcDirs = ((tempDir ++ "/") ++) <$> ("." : hsSourceDirs (libBuildInfo libData))
      moduleNames = exposedModules libData

  go <- async $ do
    let packageArchive = tempDir ++ "/" ++ packageString ++ ".tar.gz"
    createDirectory tempDir
    LBS.writeFile packageArchive (decompress unparsed)
    extract "tmp/" packageArchive
    moduleSources' <- mapM (\m -> (m,) <$> findFile hsSrcDirs
                                             (toFilePath m ++ ".hs"))
                           moduleNames
    let moduleSources = foldr (\(m,f) acc ->
                                if isNothing f
                                then error ("Module not found - " ++ show m ++ " in " ++ show hsSrcDirs)
                                else (m,fromJust f) : acc)
                              []
                              moduleSources'
    mModules :: [ParseResult (ModuleName, Module)] <- mapM (\(m,f) -> (m,) <$$> parseFile f) moduleSources
    let modules = Map.fromList $ fromParseResult $ sequenceA mModules

    modules `seq` removeDirectoryRecursive tempDir
    return modules
  wait go

