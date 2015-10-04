{-# LANGUAGE
    OverloadedStrings
  #-}

module Fetch.Cabal where

import Fetch.Versions

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as LBS

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (normal)
import System.Directory (removeFile)
import Control.Concurrent.Async



-- | Ping the Hackage server for a list of all packages
hackagePackageDescription :: String -- ^ package name
                          -> HackageVersion
                          -> IO GenericPackageDescription
hackagePackageDescription packageName vs = do
  let packageString = packageName ++ "-" ++ showHackageVersion vs

  manager <- newManager tlsManagerSettings
  request <- parseUrl $ "https://hackage.haskell.org/package/"
                      ++ packageString
                      ++ "/" ++ packageName ++ ".cabal"

  unparsed' <- httpLbs request manager
  let unparsed = responseBody unparsed'
      tempCabalFile = "tmp/" ++ packageString ++ ".cabal"

  go <- async $ do
    LBS.writeFile tempCabalFile unparsed
    description <- readPackageDescription normal tempCabalFile
    removeFile tempCabalFile
    return description
  wait go

