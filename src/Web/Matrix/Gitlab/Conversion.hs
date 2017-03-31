{-# LANGUAGE OverloadedStrings #-}

module Web.Matrix.Gitlab.Conversion
  ( convertGitlabEvent
  ) where

import Control.Monad (mapM_)
import Data.Foldable (length, fold)
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as Text
import Data.Text.Format (format)
import Lucid
       (Html, ol_, li_, toHtml, strong_, span_, a_, href_, hr_)
import Plpd.Util (textShow, surroundQuotes, formatStrict)
import Prelude ()
import Web.Matrix.Bot.IncomingMessage
       (IncomingMessage, constructIncomingMessageLazy,
        constructIncomingMessage)
import Web.Matrix.Gitlab.API
       (GitlabEvent, GitlabCommit, eventObjectKind, eventCommits,
        eventUserName, eventUserUserName, objectNote, objectUrl,
        objectTitle, objectState,objectAction, eventIssue, issueTitle,
        eventObjectAttributes, eventRepository, commitMessage, commitUrl,
        repositoryName)

actionToText :: Text.Text -> Text.Text
actionToText t =
  case t of
    "open" -> "opened"
    "update" -> "updated"
    _ -> "changed"

convertGitlabEvent :: GitlabEvent -> (IncomingMessage Text.Text (Html ()))
convertGitlabEvent event =
  case eventObjectKind event of
    "push" ->
      let repo = fromJust (eventRepository event)
          commitCount = maybe "0" textShow (length <$> (eventCommits event))
          userName = fold (eventUserName event)
          commitHtml :: GitlabCommit -> Html ()
          commitHtml commit =
            li_
              (a_
                 [href_ (commitUrl commit)]
                 (toHtml . surroundQuotes . Text.strip . commitMessage $ commit))
          commitsHtml =
            maybe
              mempty
              (\commits -> ol_ (mapM_ commitHtml commits))
              (eventCommits event)
          commitsPlain =
            fold
              ((Text.intercalate ", " .
                ((surroundQuotes . Text.strip . commitMessage) <$>)) <$>
               eventCommits event)
          messagePlain =
            formatStrict
              "{} pushed {} commit(s) to {}: {}"
              (userName, commitCount, repositoryName repo, commitsPlain)
          messageHtml =
            (strong_ (toHtml userName)) <> " pushed " <> (toHtml commitCount) <>
            " commit(s) to " <>
            (strong_ . toHtml . repositoryName $ repo) <>
            ": " <>
            commitsHtml
      in constructIncomingMessage messagePlain (Just messageHtml)
    "issue" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          action = actionToText (fromJust (objectAction attributes))
          messagePlain =
            formatStrict
              "{} {} issue {} to {}"
              ( userName
              , action
              , surroundQuotes . fromJust . objectTitle $ attributes
              , repositoryName repo)
          issueLink =
            a_
              [href_ (objectUrl attributes)]
              (toHtml . surroundQuotes . fromJust . objectTitle $ attributes)
          messageHtml =
            strong_ (toHtml userName) <> " " <> toHtml action <> " " <> issueLink <>
            " in " <>
            strong_ (toHtml (repositoryName repo))
      in constructIncomingMessage messagePlain (Just messageHtml)
    "note" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          title = fromJust (issueTitle <$> (eventIssue event))
          attributes = fromJust (eventObjectAttributes event)
          messagePlain =
            formatStrict
              "{} commented {} to issue {} in {}"
              ( userName
              , surroundQuotes . fromJust . objectNote $ attributes
              , surroundQuotes title
              , repositoryName repo)
          issueLink =
            a_
              [href_ (objectUrl attributes)]
              (toHtml . surroundQuotes . fromJust . objectNote $ attributes)
          messageHtml =
            strong_ (toHtml userName) <> " commented " <> issueLink <> " to issue " <> toHtml (surroundQuotes title) <> " in " <> strong_ (toHtml (repositoryName repo))
      in constructIncomingMessage messagePlain (Just messageHtml)
    unknown ->
      constructIncomingMessage ("Unknown gitlab event type " <> unknown) Nothing
