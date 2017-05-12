{-# LANGUAGE DeriveGeneric #-}

module Web.Matrix.Gitlab.Internal.Project
  ( GitlabProject
  , projectName
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude ()
import Text.Show (Show)

data GitlabProject = GitlabProject
  { name :: Text
  } deriving (Generic, Show)

projectName :: GitlabProject -> Text
projectName = name

instance FromJSON GitlabProject
