{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Web.Matrix.Gitlab.ConfigOptions
  ( ConfigOptions(..)
  , readConfigOptions
  , coLogFile
  , coListenPort
  , coBotUrl
  , coRepoMapping
  ) where

import           Control.Applicative    ((<*>))
import           Control.Lens           (makeLenses)
import           Control.Lens           (Getter, to)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Function          ((.))
import           Data.Functor           ((<$>))
import           Data.Int               (Int)
import           Data.Maybe             (Maybe)
import           Data.String            (String, fromString)
import qualified Data.Text              as Text
import           Data.Text.Buildable    (build)
import           Data.Text.Lazy         (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Dhall                  as Dhall
import           GHC.Generics           (Generic)
import           Prelude                (error, fromIntegral)
import           System.FilePath        (FilePath)
import           System.IO              (IO)

data ConfigOptions = ConfigOptions
  { logFile     :: Dhall.Text
  , listenPort  :: Dhall.Natural
  , botUrl      :: Dhall.Text
  , repoMapping :: Dhall.Text
  } deriving(Generic,Dhall.Interpret)

readConfigOptions :: FilePath -> IO ConfigOptions
readConfigOptions = Dhall.detailed . Dhall.input Dhall.auto . fromString

toText :: Dhall.Text -> Text.Text
toText = toStrict . toLazyText . build

toString :: Dhall.Text -> String
toString = Text.unpack . toText

coLogFile :: Getter ConfigOptions FilePath
coLogFile = to (toString . logFile)

coListenPort :: Getter ConfigOptions Int
coListenPort = to (fromIntegral . listenPort)

coBotUrl :: Getter ConfigOptions Text.Text
coBotUrl = to (toText . botUrl)

coRepoMapping :: Getter ConfigOptions FilePath
coRepoMapping = to (toString . repoMapping)
