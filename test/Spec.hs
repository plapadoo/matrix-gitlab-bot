{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.TH (defaultMainGenerator)
import Test.HUnit(assertFailure,assertBool,assertEqual)
import Web.Matrix.Gitlab.Conversion(convertGitlabEvent)
import Web.Matrix.Gitlab.RepoMapping(readRepoMappingText,rooms,Room(..),roomsForEntity,RoomEntity(..),Repo(..))
import Web.Matrix.Bot.IncomingMessage(plainBody,markupBody)
import Prelude()
import System.IO(IO)
import Data.ByteString.Lazy(readFile)
import Data.Aeson(decode)
import Data.Functor((<$>))
import Control.Monad(return)
import Data.Maybe(Maybe(..))
import Data.Text.IO(putStrLn)
import Data.Text(isInfixOf)
import Plpd.Util(textShow)
import Control.Lens((^.))
import Data.Either(Either(..))
import Data.Function(($))
import Data.Monoid((<>))

case_simpleRepoMapping =
  case readRepoMappingText "room=repo1,repo2" of
    Left e -> assertFailure $ "error parsing repo mapping: " <> e
    Right result -> do
      assertEqual "invalid rooms" [Room "room"] (rooms result)
      assertEqual "invalid entities" [Room "room"] (roomsForEntity result (RoomToRepo (Repo "repo1" )))
      assertEqual "invalid entities" [Room "room"] (roomsForEntity result (RoomToRepo (Repo "repo2" )))

case_conversionForPushEvent = do
  gitlabEvent <- ( convertGitlabEvent <$> ) <$> decode <$> readFile "test/data/push.json"
  case gitlabEvent of
    Nothing -> assertFailure "couldn't convert input json (push)"
    Just event -> do
      putStrLn (textShow event)
      assertBool "author is preserved in plain" ("John Smith" `isInfixOf` (event ^. plainBody ))
      case event ^. markupBody of
        Nothing -> assertFailure "no markup"
        Just markup -> assertBool "author is preserved in markup" ("John Smith" `isInfixOf` (textShow markup ))

case_conversionForIssueEvent = do
  gitlabEvent <- ( convertGitlabEvent <$> ) <$> decode <$> readFile "test/data/issue.json"
  case gitlabEvent of
    Nothing -> assertFailure "couldn't convert input json (issue)"
    Just event -> do
      putStrLn (textShow event)
      assertBool "author is preserved in plain" ("Plato" `isInfixOf` (event ^. plainBody ))
      case event ^. markupBody of
        Nothing -> assertFailure "no markup"
        Just markup -> assertBool "author is preserved in markup" ("Plato" `isInfixOf` (textShow markup ))

case_conversionForUpdateEvent = do
  gitlabEvent <- ( convertGitlabEvent <$> ) <$> decode <$> readFile "test/data/check_complete.json"
  case gitlabEvent of
    Nothing -> assertFailure "couldn't convert input json (check complete)"
    Just event -> do
      putStrLn (textShow event)
      assertBool "author is preserved in plain" ("doofy" `isInfixOf` (event ^. plainBody ))
      case event ^. markupBody of
        Nothing -> assertFailure "no markup"
        Just markup -> do
          assertBool "markup contains flag" ("updated" `isInfixOf` (textShow markup))
          assertBool "author is preserved in markup" ("doofy" `isInfixOf` (textShow markup ))

case_conversionForCommentEvent = do
  gitlabEvent <- ( convertGitlabEvent <$> ) <$> decode <$> readFile "test/data/comment.json"
  case gitlabEvent of
    Nothing -> assertFailure "couldn't convert input json (push)"
    Just event -> do
      putStrLn (textShow event)
      assertBool "author is preserved in plain" ("Plato" `isInfixOf` (event ^. plainBody ))
      assertBool "commented is contained plain" ("commented" `isInfixOf` (event ^. plainBody ))
      case event ^. markupBody of
        Nothing -> assertFailure "no markup"
        Just markup -> assertBool "commented is preserved in markup" ("commented" `isInfixOf` (textShow markup))

main :: IO ()
main = $(defaultMainGenerator)
