{-# LANGUAGE TemplateHaskell #-}
module Web.Matrix.Gitlab.ProgramOptions
  (ProgramOptions(..)
  ,readProgramOptions
  ,poConfigFile)
  where

import Prelude (Int)
import qualified Options.Applicative as OptAppl
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (IO)
import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>))
import System.FilePath (FilePath)
import Control.Lens (makeLenses)

data ProgramOptions = ProgramOptions
    { _poConfigFile :: FilePath
    }

makeLenses ''ProgramOptions

programOptionsParser :: OptAppl.Parser ProgramOptions
programOptionsParser =
    ProgramOptions <$>
    OptAppl.strOption
        (OptAppl.long "config-file" <>
         OptAppl.help "Where to put the config file" <>
         OptAppl.value "/etc/matrix-bot/matrix-gitlab-bot.dhall")

readProgramOptions
    :: MonadIO m
    => m ProgramOptions
readProgramOptions = liftIO (OptAppl.execParser opts)
  where
    opts =
        OptAppl.info
            (OptAppl.helper <*> programOptionsParser)
            (OptAppl.fullDesc <>
             OptAppl.progDesc
                 "Listen for gitlab webhooks, send them to a matrix channel" <>
             OptAppl.header "gitlab-matrix-bot - send git stuff to matrix")

