{-# LANGUAGE DeriveGeneric #-}

module Web.Matrix.Gitlab.API
  ( GitlabEvent
  , eventObjectKind
  , objectState
  , eventRepository
  , eventRef
  , eventCommits
  , objectTitle
  , objectAction
  , objectUrl
  , objectNote
  , eventUserName
  , issueTitle
  , GitlabIssue
  , eventIssue
  , eventUserUserName
  , repositoryName
  , eventObjectAttributes
  , GitlabCommit
  , commitMessage
  , commitUrl
  ) where

import Data.Aeson (FromJSON)
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude ()
import Text.Show (Show)
import Web.Matrix.Gitlab.Internal.Commit
       (GitlabCommit, commitMessage, commitUrl)
import Web.Matrix.Gitlab.Internal.Issue (GitlabIssue, issueTitle)

data GitlabRepository = GitlabRepository
  { name :: Text
  } deriving (Generic, Show)

data GitlabObjectAttributes = GitlabObjectAttributes
  { title :: Maybe Text
  , url :: Text
  , note :: Maybe Text
  , state :: Maybe Text
  , action :: Maybe Text
  } deriving (Generic, Show)

objectTitle :: GitlabObjectAttributes -> Maybe Text
objectTitle = title

objectState :: GitlabObjectAttributes -> Maybe Text
objectState = state

objectNote :: GitlabObjectAttributes -> Maybe Text
objectNote = note

objectUrl :: GitlabObjectAttributes -> Text
objectUrl = url

objectAction :: GitlabObjectAttributes -> Maybe Text
objectAction = action

data GitlabUser = GitlabUser
  { username :: Text
  } deriving (Generic, Show)

data GitlabEvent = GitlabEvent
  { object_kind :: Text
  , user :: Maybe GitlabUser
  , ref :: Maybe Text
  , user_name :: Maybe Text
  , repository :: Maybe GitlabRepository
  , commits :: Maybe [GitlabCommit]
  , object_attributes :: Maybe GitlabObjectAttributes
  , issue :: Maybe GitlabIssue
  } deriving (Generic, Show)

eventObjectAttributes :: GitlabEvent -> Maybe GitlabObjectAttributes
eventObjectAttributes = object_attributes

eventObjectKind :: GitlabEvent -> Text
eventObjectKind = object_kind

eventIssue :: GitlabEvent -> Maybe GitlabIssue
eventIssue = issue

eventCommits :: GitlabEvent -> Maybe [GitlabCommit]
eventCommits = commits

eventRepository :: GitlabEvent -> Maybe GitlabRepository
eventRepository = repository

eventRef :: GitlabEvent -> Maybe Text
eventRef = ref

eventUserName :: GitlabEvent -> Maybe Text
eventUserName = user_name

eventUserUserName :: GitlabEvent -> Maybe Text
eventUserUserName event = username <$> user event

repositoryName :: GitlabRepository -> Text
repositoryName = name

instance FromJSON GitlabRepository

instance FromJSON GitlabEvent

instance FromJSON GitlabObjectAttributes

instance FromJSON GitlabUser
