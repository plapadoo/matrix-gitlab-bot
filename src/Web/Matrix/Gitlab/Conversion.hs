{-# LANGUAGE OverloadedStrings #-}

module Web.Matrix.Gitlab.Conversion
  ( convertGitlabEvent
  , IncomingMessage(..)
  ) where

import           Control.Monad         (mapM_, (>>=))
import           Data.Foldable         (fold, length)
import           Data.Function         (($), (.))
import           Data.Functor          ((<$>))
import           Data.Maybe            (Maybe (..), fromJust, fromMaybe, maybe)
import           Data.Monoid           (mempty, (<>))
import qualified Data.Text             as Text
import           Data.Text.Format      (format)
import           Data.Tuple            (snd)
import           Lucid                 (Html, a_, em_, hr_, href_, li_, ol_,
                                        span_, strong_, toHtml)
import           Plpd.Util             (formatStrict, surroundQuotes, textShow)
import           Prelude               ()
import           Text.Show             (Show)
import           Web.Matrix.Gitlab.API (GitlabCommit, GitlabEvent,
                                        commitMessage, commitUrl, eventCommits,
                                        eventIssue, eventObjectAttributes,
                                        eventObjectKind, eventProject, eventRef,
                                        eventRepository, eventUserName,
                                        eventUserUserName, issueTitle,
                                        objectAction, objectId, objectNote,
                                        objectState, objectStatus, objectTitle,
                                        objectUrl, projectName, repositoryName)

actionToText :: Text.Text -> Text.Text
actionToText t =
  case t of
    "open"   -> "opened"
    "close"  -> "closed"
    "reopen" -> "reopened"
    "update" -> "updated"
    _        -> "changed"

resolveRefBranch :: Text.Text -> Text.Text
resolveRefBranch = snd . Text.breakOnEnd "/"

data IncomingMessage = IncomingMessage Text.Text (Maybe (Html ()))
                     deriving(Show)

convertGitlabEvent :: GitlabEvent -> IncomingMessage
convertGitlabEvent event =
  case eventObjectKind event of
    "pipeline" ->
      let name = projectName ( fromJust (eventProject event) )
          status = fromJust ( eventObjectAttributes event >>= objectStatus )
          pipelineId = fromJust ( eventObjectAttributes event >>= objectId )
          htmlStatus =
            case status of
              "success" -> "👍 success"
              "pending" -> "💤 pending"
              "running" -> "🏃 running"
              "failed"  -> "⚠ failed"
              x         -> toHtml status
          messagePlain = "🔧 pipeline " <> textShow pipelineId <> " in project " <> name <> " " <> status
          messageHtml = "🔧 pipeline " <> toHtml (textShow pipelineId) <> " in project " <> strong_ (toHtml name) <> " " <> strong_ htmlStatus
      in IncomingMessage messagePlain (Just messageHtml)
    "push" ->
      let repo = fromJust (eventRepository event)
          commitCount = maybe "0" textShow (length <$> eventCommits event)
          userName = fold (eventUserName event)
          branch = maybe "master" resolveRefBranch (eventRef event)
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
              "🔁 {} pushed {} commit(s) to {}/{}: {}"
              (userName, commitCount, repositoryName repo, branch, commitsPlain)
          messageHtml =
            "🔁 " <> (strong_ (toHtml userName)) <> " pushed " <> (toHtml commitCount) <>
            " commit(s) to " <>
            (strong_ . toHtml . repositoryName $ repo) <>
            "/" <>
            (strong_ . toHtml $ branch) <>
            ": " <>
            commitsHtml
      in IncomingMessage messagePlain (Just messageHtml)
    "issue" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          action = actionToText (fromJust (objectAction attributes))
          messagePlain =
            formatStrict
              "📋 {} {} issue {} in {}"
              ( userName
              , action
              , surroundQuotes . fromJust . objectTitle $ attributes
              , repositoryName repo)
          issueLink =
            a_
              [href_ (fromJust (objectUrl attributes))]
              (toHtml . surroundQuotes . fromJust . objectTitle $ attributes)
          messageHtml =
            "📋 " <> strong_ (toHtml userName) <> " " <> toHtml action <> " " <> issueLink <>
            " in " <>
            strong_ (toHtml (repositoryName repo))
      in IncomingMessage messagePlain (Just messageHtml)
    "note" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          title = fromJust (issueTitle <$> eventIssue event)
          attributes = fromJust (eventObjectAttributes event)
          messagePlain =
            formatStrict
              "💬 {} commented {} to issue {} in {}"
              ( userName
              , surroundQuotes . fromJust . objectNote $ attributes
              , surroundQuotes title
              , repositoryName repo)
          issueLink =
            a_
              [href_ (fromJust (objectUrl attributes))]
              (toHtml . surroundQuotes . fromJust . objectNote $ attributes)
          messageHtml =
            "💬 " <> strong_ (toHtml userName) <> " commented " <> issueLink <> " to issue " <> toHtml (surroundQuotes title) <> " in " <> strong_ (toHtml (repositoryName repo))
      in IncomingMessage messagePlain (Just messageHtml)
    unknown ->
      IncomingMessage ("Unknown gitlab event type " <> unknown) Nothing
