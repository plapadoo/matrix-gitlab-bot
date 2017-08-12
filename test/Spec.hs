{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Lens                   ((^.))
import           Control.Monad                  (return)
import           Data.Aeson                     (decode)
import           Data.ByteString.Lazy           (readFile)
import           Data.Either                    (Either (..))
import           Data.Function                  (($))
import           Data.Functor                   ((<$>))
import           Data.Maybe                     (Maybe (..))
import           Data.Monoid                    ((<>))
import           Data.Text                      (isInfixOf)
import           Data.Text.IO                   (putStrLn)
import           Plpd.Util                      (textShow)
import           Prelude                        ()
import           System.IO                      (IO)
import           Test.Framework                 (defaultMain)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.TH              (defaultMainGenerator)
import           Test.HUnit                     (assertBool, assertEqual,
                                                 assertFailure)
import           Web.Matrix.Bot.IncomingMessage (markupBody, plainBody)
import           Web.Matrix.Gitlab.Conversion   (convertGitlabEvent)

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
