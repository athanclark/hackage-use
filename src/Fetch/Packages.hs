{-# LANGUAGE
    OverloadedStrings
  #-}

module Fetch.Packages where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import qualified Data.Text as T



newtype HackagePackage = HackagePackage
  { packageName :: T.Text
  } deriving (Show, Eq)

instance FromJSON HackagePackage where
  parseJSON (Object xs) = HackagePackage <$> xs .: "packageName"
  parseJSON _ = fail "Non-object provided to HackagePackage parser"

-- | Ping the Hackage server for a list of all packages
hackagePackages :: IO [HackagePackage]
hackagePackages = do
  manager <- newManager tlsManagerSettings

  request' <- parseUrl "https://hackage.haskell.org/packages/"
  let request = request' { requestHeaders = [("Accept", "application/json")]
                         }

  unparsed' <- httpLbs request manager
  let unparsed = responseBody unparsed'
      parsed = eitherDecode unparsed :: Either String [HackagePackage]
  case parsed of
    Left e -> error e
    Right r -> return r


