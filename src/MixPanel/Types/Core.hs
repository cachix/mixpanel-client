module MixPanel.Types.Core (AuthToken(..), IsSuccess(..)) where

import Data.Aeson (ToJSON, FromJSON, parseJSON, toJSON, withScientific)
import Data.Text       (Text)
import GHC.Generics (Generic)
import Servant.API

newtype AuthToken = AuthToken Text
  deriving (Generic, ToJSON)

data IsSuccess = Success | Fail
  deriving (Show)

instance ToJSON IsSuccess where
  toJSON Success = "1"
  toJSON Fail = "0"

instance FromJSON IsSuccess where
  parseJSON = withScientific "number" $ \case
    1 -> return Success
    0 -> return Fail
    y -> fail $ "IsSuccess can only be 1 or 0, not " <> show y

instance ToHttpApiData IsSuccess where
  toUrlPiece Success = "1"
  toUrlPiece Fail = "0"
