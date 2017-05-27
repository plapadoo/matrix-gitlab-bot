{-# LANGUAGE OverloadedStrings #-}

module Web.Matrix.Gitlab.Conversion
  ( convertGitlabEvent
  ) where

import Control.Monad (mapM_,(>>=))
import Data.Foldable (length, fold)
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromJust, maybe,fromMaybe)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as Text
import Data.Text.Format (format)
import Lucid
       (Html, ol_, li_, toHtml, em_,strong_, span_, a_, href_, hr_)
import Plpd.Util (textShow, surroundQuotes, formatStrict)
import Prelude ()
import Web.Matrix.Bot.IncomingMessage
       (IncomingMessage, constructIncomingMessageLazy,
        constructIncomingMessage)
import Web.Matrix.Gitlab.API
       (GitlabEvent, GitlabCommit, eventObjectKind, eventCommits,
        eventUserName, eventUserUserName, objectNote, objectUrl,
        eventProject,projectName,
        objectId,objectTitle, objectStatus,objectState,objectAction, eventIssue, issueTitle,
        eventObjectAttributes, eventRef,eventRepository, commitMessage, commitUrl,
        repositoryName)
import Data.Tuple(snd)

actionToText :: Text.Text -> Text.Text
actionToText t =
  case t of
    "open" -> "opened"
    "close" -> "closed"
    "reopen" -> "reopened"
    "update" -> "updated"
    _ -> "changed"

resolveRefBranch :: Text.Text -> Text.Text
resolveRefBranch = snd . Text.breakOnEnd "/"

convertGitlabEvent :: GitlabEvent -> (IncomingMessage Text.Text (Html ()))
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
              "failed" -> "⚠ failed"
              x -> toHtml status
          messagePlain = "🔧 pipeline " <> textShow pipelineId <> " in project " <> name <> " " <> status
          messageHtml = "🔧 pipeline " <> toHtml (textShow pipelineId) <> " in project " <> strong_ (toHtml name) <> " " <> (strong_ htmlStatus) 
      in constructIncomingMessage messagePlain (Just messageHtml)
    "push" ->
      let repo = fromJust (eventRepository event)
          commitCount = maybe "0" textShow (length <$> (eventCommits event))
          userName = fold (eventUserName event)
          branch = fromMaybe "master" (resolveRefBranch <$> (eventRef event))
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
      in constructIncomingMessage messagePlain (Just messageHtml)
    "issue" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          action = actionToText (fromJust (objectAction attributes))
          messagePlain =
            formatStrict
              "📋 {} {} issue {} to {}"
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
      in constructIncomingMessage messagePlain (Just messageHtml)
    "note" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          title = fromJust (issueTitle <$> (eventIssue event))
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
      in constructIncomingMessage messagePlain (Just messageHtml)
    unknown ->
      constructIncomingMessage ("Unknown gitlab event type " <> unknown) Nothing
