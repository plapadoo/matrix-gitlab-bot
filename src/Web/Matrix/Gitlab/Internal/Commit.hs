{-# LANGUAGE DeriveGeneric #-}

module Web.Matrix.Gitlab.Internal.Commit
  ( GitlabCommit
  , commitMessage
  , commitUrl
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude ()
import Text.Show (Show)

data GitlabCommit = GitlabCommit
  { message :: Text
  , url :: Text
  } deriving (Generic, Show)

commitMessage :: GitlabCommit -> Text
commitMessage = message

commitUrl :: GitlabCommit -> Text
commitUrl = url

instance FromJSON GitlabCommit
