{-# LANGUAGE OverloadedStrings #-}

module GMB.Gitlab.Conversion
  ( convertGitlabEvent
  ) where

import Data.Foldable (length, fold)
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text.Format (format)
import GMB.Gitlab.API
       (GitlabEvent, eventObjectKind, eventCommits, eventUserName,
        eventUserUserName, objectNote, objectUrl, objectTitle, objectState,
        eventObjectAttributes, eventRepository, commitMessage,
        repositoryName)
import GMB.IncomingMessage
       (IncomingMessage, constructIncomingMessageLazy,
        constructIncomingMessage)
import Plpd.Util (textShow, surroundQuotes, surroundHtml)
import Prelude ()

convertGitlabEvent :: GitlabEvent -> IncomingMessage
convertGitlabEvent event =
  case eventObjectKind event of
    "push" ->
      let repo = fromJust (eventRepository event)
          commitCount = maybe "0" textShow (length <$> (eventCommits event))
          userName = fold (eventUserName event)
          commits =
            fold
              ((Text.intercalate ", " .
                ((surroundQuotes . Text.strip . commitMessage) <$>)) <$>
               eventCommits event)
          messageFormat = format "{} pushed {} commit(s) to {}: {}"
          message =
            messageFormat (userName, commitCount, repositoryName repo, commits)
          formattedMessage =
            messageFormat
              ( surroundHtml "strong" userName
              , commitCount
              , surroundHtml "strong" (repositoryName repo)
              , commits)
      in constructIncomingMessageLazy message (Just formattedMessage)
    "issue" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          state = fromJust (objectState attributes)
          message =
            format
              "{} {} issue {} to {}"
              ( userName
              , state
              , surroundQuotes . fromJust . objectTitle $ attributes
              , repositoryName repo)
          issueLink =
            format
              "<a href=\"{}\">{}</a>"
              ( objectUrl attributes
              , surroundQuotes . fromJust . objectTitle $ attributes)
          formattedMessage =
            format
              "{} {} issue {} to {}"
              ( surroundHtml "strong" userName
              , state
              , issueLink
              , surroundHtml "strong" (repositoryName repo))
      in constructIncomingMessageLazy message (Just formattedMessage)
    "note" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          message =
            format
              "{} commented {} to {}"
              ( userName
              , surroundQuotes . fromJust . objectNote $ attributes
              , repositoryName repo)
          issueLink =
            format
              "<a href=\"{}\">{}</a>"
              ( objectUrl attributes
              , surroundQuotes . fromJust . objectNote $ attributes)
          formattedMessage =
            format
              "{} commented {} to {}"
              ( surroundHtml "strong" userName
              , issueLink
              , surroundHtml "strong" (repositoryName repo))
      in constructIncomingMessageLazy message (Just formattedMessage)
    unknown ->
      constructIncomingMessage ("Unknown gitlab event type " <> unknown) Nothing
