{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Applicative (Applicative)
import Control.Lens ((^.), view)
import Control.Monad (return, Monad, mapM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.Aeson (decode)
import Data.Bool (Bool(True))
import Data.ByteString.Lazy (toStrict)
import Data.Either (Either(..))
import Data.Foldable (and)
import Data.Function (($))
import Data.Functor (Functor, (<$>))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import GMB.API (sendMessage)
import GMB.Gitlab.API
       (GitlabEvent, eventRepository, repositoryName)
import GMB.Gitlab.ConfigOptions
       (ConfigOptions, readConfigOptions, coListenPort, coBotUrl,
        coLogFile, coRepoMapping)
import GMB.Gitlab.Conversion (convertGitlabEvent)
import GMB.Gitlab.ProgramOptions (readProgramOptions, poConfigFile)
import GMB.Gitlab.RepoMapping
       (readRepoMapping, Repo(..), roomsForRepo, Room(..))
import Network.HTTP.Types.Status (internalServerError500, ok200)
import Plpd.Http (MonadHttp(..), loggingHttp)
import Plpd.MonadLog (MonadLog(..), defaultLog)
import Plpd.Util (textShow)
import Prelude ()
import System.IO (IO)
import Web.Scotty (scotty, post, status, body)

newtype MyMonad a = MyMonad
  { runMyMonad :: ReaderT ConfigOptions IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ConfigOptions)

instance MonadLog MyMonad where
  putLog inputText = do
    logFile <- view coLogFile
    defaultLog logFile inputText

instance MonadHttp MyMonad where
  httpRequest = loggingHttp

main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  rm' <- readRepoMapping (configOptions ^. coRepoMapping)
  let defLog = defaultLog (configOptions ^. coLogFile)
  case rm' of
    Left e ->
      defaultLog (configOptions ^. coLogFile) $
      "error reading repo mapping: " <> (pack e)
    Right rm ->
      scotty (configOptions ^. coListenPort) $
      post "/" $ do
        content <- body
        defLog ("Got JSON data: " <> (decodeUtf8 (toStrict content)))
        case decode content of
          Nothing -> defLog "couldn't parse json"
          Just decodedJson ->
            case repositoryName <$> eventRepository decodedJson of
              Nothing -> defLog "No repository found in JSON"
              Just repo -> do
                let incomingMessage = convertGitlabEvent decodedJson
                    sendToRoom room =
                      sendMessage
                        (configOptions ^. coBotUrl)
                        room
                        incomingMessage
                result <-
                  liftIO
                    (runReaderT
                       (runMyMonad
                          (mapM
                             (\(Room r) -> sendToRoom r)
                             (roomsForRepo rm (Repo repo))))
                       configOptions)
                if and result
                  then status ok200
                  else status internalServerError500
