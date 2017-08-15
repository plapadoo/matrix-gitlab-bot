{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Web.Matrix.Gitlab.RepoMapping
  ( RepoMapping
  , readRepoMapping
  , roomsForRepo
  , Room(..)
  , RepoMappings
  , Repo(..)
  , rooms
  ) where

import           Control.Applicative    ((*>), (<*), (<*>))
import           Control.Monad          (return)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bool              (Bool, otherwise, (||))
import           Data.Char              (Char)
import           Data.Either            (Either)
import           Data.Eq                (Eq, (/=), (==))
import           Data.Foldable          (concatMap, foldMap,toList)
import           Data.Function          ((.))
import           Data.Functor           ((<$>))
import           Data.List              (filter)
import           Data.Ord               (Ord)
import           Data.String            (String, fromString)
import qualified Data.Text              as Text
import           Data.Text.IO           (readFile,putStrLn)
import           Data.Tuple             (fst, snd)
import qualified Dhall                  as Dhall
import           GHC.Generics           (Generic)
import           Plpd.Dhall             (toString, toText)
import           Prelude                (undefined)
import           System.FilePath        (FilePath)
import           Text.Show              (Show,show)

data RepoMapping = RepoMapping {
    room :: Dhall.Text
  , repo :: Dhall.Text
  } deriving(Generic,Dhall.Interpret)

newtype Room = Room { roomText :: Text.Text } deriving(Eq)
newtype Repo = Repo { repoText :: Text.Text } deriving(Eq)

instance Show Room where
  show (Room a) = Text.unpack a

instance Show Repo where
  show (Repo a) = Text.unpack a

type RepoMappings = Dhall.Vector RepoMapping

rooms :: RepoMappings -> [Room]
rooms rm = toList ((Room . toText . room) <$> rm)

repoMappingToList :: RepoMappings -> [(Room, Repo)]
repoMappingToList rm = toList ((\(RepoMapping room repo) -> (Room (toText room),Repo (toText repo))) <$> rm)

roomsForRepo :: RepoMappings -> Repo -> [Room]
roomsForRepo rm repo = fst <$> (filter ((== repo) . snd) (repoMappingToList rm))

inputLifted x = liftIO (Dhall.detailed (Dhall.input Dhall.auto (fromString x)))

readRepoMapping :: MonadIO m => FilePath -> m RepoMappings
readRepoMapping fn = do
  fc <- liftIO (readFile fn)
  liftIO (putStrLn fc)
  inputLifted fn
