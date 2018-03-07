{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import           Control.Applicative              (Applicative, (<*>), (<|>))
import           Control.Concurrent.MVar          (MVar, modifyMVar_, newMVar)
import           Control.Exception                (IOException, try)
import           Control.Lens                     (makeLenses, view, (^.))
import           Control.Monad                    (Monad, mapM, return)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader, ReaderT,
                                                   runReaderT)
import           Data.Aeson                       (decode)
import           Data.Bool                        (Bool (False, True))
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Either                      (Either (..))
import           Data.Foldable                    (and, foldMap, null)
import           Data.Function                    (const, id, ($), (.))
import           Data.Functor                     (Functor, (<$>))
import           Data.Maybe                       (Maybe (..))
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as Text
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy                   as LazyText
import           Lucid                            (body_, renderText)
import           Network.HTTP.Types.Status        (badRequest400,
                                                   internalServerError500,
                                                   notImplemented501, ok200,
                                                   serviceUnavailable503)
import           Plpd.Http                        (HttpMethod (HttpMethodPost),
                                                   HttpRequest (..),
                                                   HttpResponse (..),
                                                   loggingHttp)
import           Plpd.MonadLog                    (LogMode (..), defaultLog)
import           Plpd.Util                        (textShow)
import           Prelude                          ()
import           System.IO                        (IO)
import           Text.Show                        (show)
import           Web.Matrix.Gitlab.API            (GitlabEvent, eventProject,
                                                   eventRepository, projectName,
                                                   repositoryName)
import           Web.Matrix.Gitlab.ConfigOptions  (ConfigOptions, coBotUrl,
                                                   coListenPort, coRepoMapping,
                                                   readConfigOptions)
import           Web.Matrix.Gitlab.Conversion     (IncomingMessage (..),
                                                   convertGitlabEvent)
import           Web.Matrix.Gitlab.ProgramOptions (poConfigFile,
                                                   readProgramOptions)
import           Web.Matrix.Gitlab.RepoMapping    (Repo (..), RepoMappings,
                                                   Room (..),
                                                   maybeReadRepoMapping,
                                                   roomsForRepo)
import           Web.Scotty                       (body, post, scotty, status)

responseToBool :: HttpResponse BSL.ByteString -> Bool
responseToBool (HttpResponse 200 "success") = True
responseToBool _                            = False

incomingMessageToText :: IncomingMessage -> Text.Text
incomingMessageToText (IncomingMessage plain markup) = (foldMap id (LazyText.toStrict . renderText . body_ <$> markup)) <> plain

sendMessage :: MonadIO m => Text.Text -> Text.Text -> IncomingMessage -> m Bool
sendMessage url room incomingMessage =
  let request = HttpRequest {
          _hrUrl = url <> "/" <> room
        , _hrMethod = HttpMethodPost
        , _hrContentType = "text/plain"
        , _hrContent = BSL.fromStrict . encodeUtf8 . incomingMessageToText $ incomingMessage
        }
  in responseToBool <$> liftIO (loggingHttp request)

main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  defaultLog LogStdout "starting..."
  scotty (configOptions ^. coListenPort) $
    post "/" $ do
      defaultLog LogStdout "got request, processing..."
      rm' <- liftIO (maybeReadRepoMapping (configOptions ^. coRepoMapping))
      defaultLog LogStdout "read repo mapping, checking if everything's fine"
      case rm' of
        Left e -> do
          defaultLog LogStderr $ "error reading repo mapping: " <> Text.pack e
          status serviceUnavailable503
        Right rm -> do
          content <- body
          defaultLog LogStdout ("Got JSON data: " <> decodeUtf8 (BSL.toStrict content))
          case decode content :: Maybe GitlabEvent of
            Nothing -> do
              defaultLog LogStderr "couldn't parse json"
              status badRequest400
            Just decodedJson -> do
              defaultLog LogStdout "parsed json"
              case repositoryName <$> eventRepository decodedJson <|> (projectName <$> eventProject decodedJson) of
                Nothing -> do
                  status notImplemented501
                  defaultLog LogStderr "No repository found in JSON"
                Just repo -> do
                  let incomingMessage = convertGitlabEvent decodedJson
                      sendToRoom room =
                        sendMessage
                          (configOptions ^. coBotUrl)
                          room
                          incomingMessage
                      rooms = roomsForRepo rm (Repo repo)
                  if null rooms
                    then defaultLog LogStderr "No rooms to send"
                    else defaultLog LogStdout $ "Sending to the following rooms: " <> textShow rooms
                  result <- mapM (\(Room r) -> sendToRoom r) rooms
                  if and result
                    then status ok200
                    else do
                      defaultLog LogStderr "Request did not work, returning 500"
                      status internalServerError500
