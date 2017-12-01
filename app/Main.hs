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
import           Data.Bool                        (Bool (True))
import           Data.ByteString.Lazy             (toStrict)
import           Data.Either                      (Either (..))
import           Data.Foldable                    (and, null)
import           Data.Function                    (const, ($), (.))
import           Data.Functor                     (Functor, (<$>))
import           Data.Maybe                       (Maybe (..))
import           Data.Monoid                      ((<>))
import           Data.Text                        (pack)
import           Data.Text.Encoding               (decodeUtf8)
import           Network.HTTP.Types.Status        (badRequest400,
                                                   internalServerError500,
                                                   ok200, serviceUnavailable503)
import           Plpd.Http                        (MonadHttp (..), loggingHttp)
import           Plpd.MonadLog                    (MonadLog (..), defaultLog)
import           Plpd.Util                        (textShow)
import           Prelude                          ()
import           System.IO                        (IO)
import           Text.Show                        (show)
import           Web.Matrix.Bot.API               (sendMessage)
import           Web.Matrix.Gitlab.API            (GitlabEvent, eventProject,
                                                   eventRepository, projectName,
                                                   repositoryName)
import           Web.Matrix.Gitlab.ConfigOptions  (ConfigOptions, coBotUrl,
                                                   coListenPort, coLogFile,
                                                   coRepoMapping,
                                                   readConfigOptions)
import           Web.Matrix.Gitlab.Conversion     (convertGitlabEvent)
import           Web.Matrix.Gitlab.ProgramOptions (poConfigFile,
                                                   readProgramOptions)
import           Web.Matrix.Gitlab.RepoMapping    (Repo (..), RepoMappings,
                                                   Room (..), readRepoMapping,
                                                   roomsForRepo)
import           Web.Scotty                       (body, post, scotty, status)

data MyDynamicState = MyDynamicState {
    _dynStateConfigOptions :: ConfigOptions
  , _dynStateLogMVar       :: MVar ()
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
  defaultLog (configOptions ^. coLogFile) "starting..."
  mvar <- newMVar ()
  let dynState = MyDynamicState configOptions mvar
      defLog x = myDefaultLog x (configOptions ^. coLogFile) mvar
  scotty (configOptions ^. coListenPort) $
    post "/" $ do
      defaultLog (configOptions ^. coLogFile) "got request, processing..."
      rm' <- liftIO $ (try (readRepoMapping (configOptions ^. coRepoMapping)) :: IO (Either IOException RepoMappings))
      case rm' of
        Left e -> do
          defaultLog (configOptions ^. coLogFile) $ "error reading repo mapping: " <> textShow e
          status serviceUnavailable503
        Right rm -> do
          content <- body
          defLog ("Got JSON data: " <> (decodeUtf8 (toStrict content)))
          case decode content :: Maybe GitlabEvent of
            Nothing -> do
              defLog "couldn't parse json"
              status badRequest400
            Just decodedJson -> do
              defLog "parsed json"
              status ok200

      --         case (repositoryName <$> eventRepository decodedJson <|> (projectName <$> eventProject decodedJson)) of
      --           Nothing -> defLog "No repository found in JSON"
      --           Just repo -> do
      --             let incomingMessage = convertGitlabEvent decodedJson
      --                 sendToRoom room =
      --                   sendMessage
      --                     (configOptions ^. coBotUrl)
      --                     room
      --                     incomingMessage
      --                 rooms = roomsForRepo rm (Repo repo)
      --             if null rooms
      --               then defLog "No rooms to send"
      --               else defLog $ "Sending to the following rooms: " <> textShow rooms
      --             result <-
      --               liftIO
      --                 (runReaderT
      --                   (runMyMonad
      --                       (mapM
      --                         (\(Room r) -> sendToRoom r)
      --                         rooms))
      --                   dynState)
      --             if and result
      --               then status ok200
      --               else do
      --                 defLog "Request did not work, returning 500"
      --                 status internalServerError500
