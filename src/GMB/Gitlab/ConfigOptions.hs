{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GMB.Gitlab.ConfigOptions
  ( ConfigOptions(..)
  , readConfigOptions
  , coLogFile
  , coRepoMapping
  , coListenPort
  , coBotUrl
  ) where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Configurator (Worth(..), load, lookup, require)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe)
import qualified Data.Text as Text
import Prelude (error, undefined)
import System.FilePath(FilePath)

data ConfigOptions = ConfigOptions
  { _coLogFile :: FilePath
  , _coListenPort :: Int
  , _coBotUrl :: Text.Text
  , _coRepoMapping :: FilePath
  }

makeLenses ''ConfigOptions

requireLift config key = liftIO (require config key)

readConfigOptions
  :: MonadIO m
  => FilePath -> m ConfigOptions
readConfigOptions configFilePath = do
  config <- liftIO (load [Required configFilePath])
  ConfigOptions <$> (requireLift config "log-file") <*> (requireLift config "listen-port") <*> (requireLift config "bot-url") <*> (requireLift config "repo-mapping")
