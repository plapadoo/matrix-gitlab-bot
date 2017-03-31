{-# LANGUAGE DeriveGeneric #-}

module Web.Matrix.Gitlab.Internal.Issue
  ( GitlabIssue
  , issueTitle
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude ()
import Text.Show (Show)

data GitlabIssue = GitlabIssue
  { title :: Text
  } deriving (Generic, Show)

issueTitle :: GitlabIssue -> Text
issueTitle = title

instance FromJSON GitlabIssue
