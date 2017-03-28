{-# LANGUAGE DeriveGeneric #-}

module GMB.Gitlab.API
  ( GitlabEvent
  , eventObjectKind
  , objectState
  , eventRepository
  , eventCommits
  , objectTitle
  , objectUrl
  , objectNote
  , eventUserName
  , eventUserUserName
  , repositoryName
  , commitMessage
  , eventObjectAttributes
  ) where

import Data.Aeson (FromJSON)
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude ()
import Text.Show (Show)

data GitlabRepository = GitlabRepository
  { name :: Text
  } deriving (Generic, Show)

data GitlabCommit = GitlabCommit
  { message :: Text
  } deriving (Generic, Show)

commitMessage :: GitlabCommit -> Text
commitMessage = message

data GitlabObjectAttributes = GitlabObjectAttributes
  { title :: Maybe Text
  , url :: Text
  , note :: Maybe Text
  , state :: Maybe Text
  } deriving (Generic,Show)

objectTitle :: GitlabObjectAttributes -> Maybe Text
objectTitle = title

objectState :: GitlabObjectAttributes -> Maybe Text
objectState = state

objectNote :: GitlabObjectAttributes -> Maybe Text
objectNote = note

objectUrl :: GitlabObjectAttributes -> Text
objectUrl = url

data GitlabUser = GitlabUser
  { username :: Text
  } deriving (Generic,Show)

data GitlabEvent = GitlabEvent
  { object_kind :: Text
  , user :: Maybe GitlabUser
  , user_name :: Maybe Text
  , repository :: Maybe GitlabRepository
  , commits :: Maybe [GitlabCommit]
  , object_attributes :: Maybe GitlabObjectAttributes
  } deriving (Generic,Show)

eventObjectAttributes :: GitlabEvent -> Maybe GitlabObjectAttributes
eventObjectAttributes = object_attributes

eventObjectKind :: GitlabEvent -> Text
eventObjectKind = object_kind

eventCommits :: GitlabEvent -> Maybe [GitlabCommit]
eventCommits = commits

eventRepository :: GitlabEvent -> Maybe GitlabRepository
eventRepository = repository

eventUserName :: GitlabEvent -> Maybe Text
eventUserName = user_name

eventUserUserName :: GitlabEvent -> Maybe Text
eventUserUserName event = username <$> user event

repositoryName :: GitlabRepository -> Text
repositoryName = name

instance FromJSON GitlabCommit

instance FromJSON GitlabRepository

instance FromJSON GitlabEvent

instance FromJSON GitlabObjectAttributes

instance FromJSON GitlabUser
