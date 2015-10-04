{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}

module Fetch.Versions where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson                     as Aeson
import qualified Data.Text            as T
import Data.Attoparsec.Text           as Atto


newtype HackageVersion = HackageVersion
  { packageVersion :: [Int]
  } deriving (Show, Eq)

parseHackageVersion :: Atto.Parser HackageVersion
parseHackageVersion =
  let integer :: Atto.Parser Int
      integer = read <$> many1 digit
  in HackageVersion <$> integer `sepBy1` (char '.')

showHackageVersion :: HackageVersion -> String
showHackageVersion (HackageVersion vs) = intercalate "." (show <$> vs)


data HackageVersions = HackageVersions
  { normalVersions     :: [HackageVersion]
  , deprecatedVersions :: [HackageVersion]
  } deriving (Show, Eq)

instance FromJSON HackageVersions where
  parseJSON (Object xs) = do
    nvs' :: [T.Text] <- xs .: "normal-version"
    dvs' :: [T.Text] <- xs .: "deprecated-version"
    let evs = do
          nvs <- traverse (parseOnly parseHackageVersion) nvs'
          dvs <- traverse (parseOnly parseHackageVersion) dvs'
          return (HackageVersions nvs dvs)
    case evs of
      Left e -> fail $ "Error occurred during HackageVersion attoparsec parser - " ++ e
      Right r -> return r
  parseJSON _ = fail "Non-object provided to HackageVersions parser"


-- | Ping the Hackage server for a list of all the version numbers for a particular pacakge.
hackageVersions :: String -- ^ Package name
                -> IO HackageVersions
hackageVersions package = do
  manager <- newManager tlsManagerSettings

  request' <- parseUrl $ "https://hackage.haskell.org/package/" ++ package ++ "/preferred"
  let request = request' { requestHeaders = [("Accept", "application/json")]
                         }

  unparsed' <- httpLbs request manager
  let unparsed = responseBody unparsed'
      parsed = eitherDecode unparsed :: Either String HackageVersions
  case parsed of
    Left e -> error e
    Right r -> return r


