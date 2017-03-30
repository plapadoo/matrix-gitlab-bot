{-# LANGUAGE OverloadedStrings #-}

module Web.Matrix.Gitlab.Conversion
  ( convertGitlabEvent
  ) where

import Data.Foldable (length, fold)
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid ((<>),mempty)
import Control.Monad(mapM_)
import qualified Data.Text as Text
import Data.Text.Format (format)
import Web.Matrix.Gitlab.API
       (GitlabEvent, eventObjectKind, eventCommits, eventUserName,
        eventUserUserName, objectNote, objectUrl, objectTitle, objectState,
        eventObjectAttributes, eventRepository, commitMessage,
        repositoryName)
import Web.Matrix.Bot.IncomingMessage
       (IncomingMessage, constructIncomingMessageLazy,
        constructIncomingMessage)
import Plpd.Util (textShow, surroundQuotes, formatStrict)
import Prelude ()
import Lucid(Html,ol_,li_,toHtml,strong_,span_,a_,href_,hr_)

convertGitlabEvent :: GitlabEvent -> (IncomingMessage Text.Text (Html ()))
convertGitlabEvent event =
  case eventObjectKind event of
    "push" ->
      let repo = fromJust (eventRepository event)
          commitCount = maybe "0" textShow (length <$> (eventCommits event))
          userName = fold (eventUserName event)
          commitsHtml =
            maybe mempty (\commits -> ol_ (mapM_ (li_ . toHtml . surroundQuotes . Text.strip . commitMessage) commits)) (eventCommits event)
          commitsPlain =
            fold
              ((Text.intercalate ", " .
                ((surroundQuotes . Text.strip . commitMessage) <$>)) <$>
               eventCommits event)
          messagePlain = formatStrict "{} pushed {} commit(s) to {}: {}" (userName, commitCount, repositoryName repo, commitsPlain)
          messageHtml =
            (strong_ (toHtml userName)) <> " pushed " <> (toHtml (textShow commitCount)) <> " commit(s) to " <> (strong_ . toHtml . repositoryName $ repo) <> ": " <> commitsHtml
      in constructIncomingMessage messagePlain (Just messageHtml)
    "issue" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          state = fromJust (objectState attributes)
          messagePlain =
            formatStrict
              "{} {} issue {} to {}"
              ( userName
              , state
              , surroundQuotes . fromJust . objectTitle $ attributes
              , repositoryName repo)
          issueLink =
            a_ [href_ (objectUrl attributes)] (toHtml . surroundQuotes . fromJust . objectTitle $ attributes)
          messageHtml =
            strong_ (toHtml userName) <> toHtml state <> issueLink <> strong_ (toHtml (repositoryName repo))
      in constructIncomingMessage messagePlain (Just messageHtml)
    "note" ->
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          messagePlain =
            formatStrict
              "{} commented {} to {}"
              ( userName
              , surroundQuotes . fromJust . objectNote $ attributes
              , repositoryName repo)
          issueLink =
            a_ [href_ (objectUrl attributes)] (toHtml . surroundQuotes . fromJust . objectNote $ attributes)
          messageHtml =
            strong_ (toHtml userName) <> " commented " <> issueLink <> " to " <> strong_ (toHtml (repositoryName repo))
      in constructIncomingMessage messagePlain (Just messageHtml)
    unknown ->
      constructIncomingMessage ("Unknown gitlab event type " <> unknown) Nothing
