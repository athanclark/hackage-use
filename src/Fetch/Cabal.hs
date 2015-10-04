{-# LANGUAGE
    OverloadedStrings
  #-}

module Fetch.Cabal where

import Fetch.Versions

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as LBS

import Distribution.PackageDescription.Parse


instance FromJSON HackagePackage where
  parseJSON (Object xs) = HackagePackage <$> xs .: "packageName"
  parseJSON _ = fail "Non-object provided to HackagePackage parser"

-- | Ping the Hackage server for a list of all packages
hackagePackages :: String -- ^ package name
                -> HackageVersion
                -> IO [GenericPackageDescription]
hackagePackages packageName vs = do
  let packageString = packageName ++ "-" ++ showHackageVersion vs

  manager <- newManager tlsManagerSettings
  request <- parseUrl $ "https://hackage.haskell.org/package/"
                      ++ packageString
                      ++ "/" ++ packageName ++ ".cabal"

  unparsed' <- httpLbs request manager
  let unparsed = responseBody unparsed'
      tempCabalFile = "tmp/" ++ packageString ++ ".cabal"

  LBS.writeFile tempCabalFile unparsed

  description <- readPackageDescription normal tempCabalFile
  return description

