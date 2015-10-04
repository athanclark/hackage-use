{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Main where

import Fetch
import           Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A

import System.Directory
import GHC.Generics
import Path

import Data.Maybe
import Data.Default
import Data.Monoid
import Control.Monad.Reader


-- * Options Parsing

-- | Application-wide options
data AppOpts = AppOpts
  { example :: Maybe String
  } deriving Generic

instance Monoid AppOpts where
  mempty = AppOpts Nothing
  mappend (AppOpts e1) (AppOpts e2) =
    AppOpts (getLast $ (Last e1) <> (Last e2))

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions

instance Default AppOpts where
  def = AppOpts (Just "example, wow!")

appOpts :: Parser AppOpts
appOpts = AppOpts
  <$> optional ( option auto
        ( long "example"
       <> short 'e'
       <> metavar "EXAMPLE"
       <> help "example argument - DEF: 'example, wow!'" ))

-- | Command-line options
data App = App
  { options :: AppOpts
  , configPath :: Maybe String }

app :: Parser App
app = App
  <$> appOpts
  <*> optional ( strOption
        ( long "config"
       <> short 'c'
       <> metavar "CONFIG"
       <> help "absolute path to config file - DEF: `pwd`/config/app.yaml" ))


-- * Executable

main :: IO ()
main = do
  (commandOpts :: App) <- execParser opts
  cwd' <- getCurrentDirectory
  let yamlConfigPath' = fromMaybe (cwd' <> "/config/app.yaml") $
        configPath commandOpts

  yamlConfigPath   <- toFilePath <$> parseAbsFile yamlConfigPath'
  yamlConfigExists <- doesFileExist yamlConfigPath
  yamlConfigContents <- if yamlConfigExists
                        then readFile yamlConfigPath
                        else return ""
  mYamlConfig <- if yamlConfigExists && yamlConfigContents /= ""
                 then Y.decodeFile yamlConfigPath
                 else return Nothing

  let yamlConfig = fromMaybe def mYamlConfig
      config = def <> yamlConfig <> options commandOpts

  runReaderT entry $ appOptsToEnv config
  where
    opts :: ParserInfo App
    opts = info (helper <*> app)
      ( fullDesc
     <> progDesc "Run executable"
     <> header "hackage-use - a Haskell application" )

entry :: ( MonadReader Env m
         , MonadIO m
         ) => m ()
entry = do
  packages <- liftIO hackagePackages
  liftIO $ print packages


-- * Utilities

data Env = Env
  { exampleEnv :: String
  } deriving (Show, Eq)

-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
appOptsToEnv :: AppOpts -> Env
appOptsToEnv (AppOpts (Just e)) =
  Env e
