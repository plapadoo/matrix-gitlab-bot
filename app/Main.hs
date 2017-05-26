{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Applicative (Applicative,(<|>),(<*>))
import Control.Lens ((^.), view,makeLenses)
import Control.Monad (return, Monad, mapM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.Aeson (decode)
import Data.Bool (Bool(True))
import Data.ByteString.Lazy (toStrict)
import Data.Either (Either(..))
import Data.Foldable (and,null)
import Data.Function (($),const,(.))
import Data.Functor (Functor, (<$>))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Web.Matrix.Bot.API (sendMessage)
import Web.Matrix.Gitlab.API
       (GitlabEvent, eventRepository, repositoryName,projectName,eventProject)
import Web.Matrix.Gitlab.ConfigOptions
       (ConfigOptions, readConfigOptions, coListenPort, coBotUrl,
        coLogFile, coRepoMapping)
import Web.Matrix.Gitlab.Conversion (convertGitlabEvent)
import Web.Matrix.Gitlab.ProgramOptions (readProgramOptions, poConfigFile)
import Web.Matrix.Gitlab.RepoMapping
       (readRepoMapping, Repo(..), roomsForRepo, Room(..))
import Network.HTTP.Types.Status (internalServerError500, ok200)
import Plpd.Http (MonadHttp(..), loggingHttp)
import Plpd.MonadLog (MonadLog(..), defaultLog)
import Plpd.Util (textShow)
import Prelude ()
import System.IO (IO)
import Web.Scotty (scotty, post, status, body)
import Control.Concurrent.MVar(MVar,newMVar,modifyMVar_)

data MyDynamicState = MyDynamicState {
    _dynStateConfigOptions :: ConfigOptions
  , _dynStateLogMVar :: MVar ()
  }

makeLenses ''MyDynamicState

newtype MyMonad a = MyMonad
  { runMyMonad :: ReaderT MyDynamicState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader MyDynamicState)

myDefaultLog inputText logFile mvar = liftIO $ modifyMVar_ mvar $ const (defaultLog logFile inputText)

instance MonadLog MyMonad where
  putLog inputText = do
    configFile <- view (dynStateConfigOptions . coLogFile)
    mvar <- view dynStateLogMVar
    myDefaultLog inputText configFile mvar

instance MonadHttp MyMonad where
  httpRequest = loggingHttp

main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  mvar <- newMVar ()
  let dynState = MyDynamicState configOptions mvar
      defLog x = myDefaultLog x (configOptions ^. coLogFile) mvar
  scotty (configOptions ^. coListenPort) $
    post "/" $ do
      rm' <- readRepoMapping (configOptions ^. coRepoMapping)
      case rm' of
        Left e ->
          defaultLog (configOptions ^. coLogFile) $ "error reading repo mapping: " <> (pack e)
        Right rm -> do
          content <- body
          defLog ("Got JSON data: " <> (decodeUtf8 (toStrict content)))
          case decode content of
            Nothing -> defLog "couldn't parse json"
            Just decodedJson ->
              case (repositoryName <$> eventRepository decodedJson <|> (projectName <$> eventProject decodedJson)) of
                Nothing -> defLog "No repository found in JSON"
                Just repo -> do
                  let incomingMessage = convertGitlabEvent decodedJson
                      sendToRoom room =
                        sendMessage
                          (configOptions ^. coBotUrl)
                          room
                          incomingMessage
                      rooms = roomsForRepo rm (Repo repo)
                  if null rooms
                    then defLog "No rooms to send"
                    else defLog $ "Sending to the following rooms: " <> textShow rooms
                  result <-
                    liftIO
                      (runReaderT
                        (runMyMonad
                            (mapM
                              (\(Room r) -> sendToRoom r)
                              rooms))
                        dynState)
                  if and result
                    then status ok200
                    else do
                      defLog "Request did not work, returning 500"
                      status internalServerError500
