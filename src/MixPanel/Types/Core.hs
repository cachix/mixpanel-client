module MixPanel.Types.Core
  ( AuthToken(..)
  , Toggle(..)
  , DidSucceed(..)
  ) where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                , (.:)
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Servant.API

newtype AuthToken = AuthToken Text
  deriving (Generic, ToJSON)

-- | MixPanel API uses boolean logic with 1 or 0,
-- | so we have a special type for it
data Toggle = On | Off
  deriving (Show)

instance ToJSON Toggle where
  toJSON On = "1"
  toJSON Off = "0"

instance ToHttpApiData Toggle where
  toUrlPiece On = "1"
  toUrlPiece Off = "0"


data DidSucceed = Success | Fail Text
  deriving (Show)

instance FromJSON DidSucceed where
  parseJSON = withObject "success or failure" $ \o -> do
    status <- o .: "status"
    case (status :: Int) of
      1 -> return Success
      0 -> do
        err <- o .: "error"
        return $ Fail err
      _ -> fail ("unknown status: " ++ show status)
