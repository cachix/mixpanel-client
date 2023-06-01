{-# LANGUAGE ExistentialQuantification #-}
module MixPanel.Types.TrackData
  ( TrackData(..)
  , mkTrackData
  , Properties(..)
  , mkProperties
  ) where

import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , encode
                                                , Value(..)
                                                , Object
                                                , (.=)
                                                )
import           Data.Aeson.Types               ( Pair )
import           Data.Text                      ( Text )
import qualified Data.ByteString.Base64.Lazy   as B64
import           Data.Time.Clock.POSIX          ( POSIXTime )
import           GHC.Exts                       ( fromList )
import           GHC.Generics                   ( Generic )
import           Servant.API
import           Data.String.Conv               ( toS )
import           MixPanel.Types.Core            ( AuthToken )


data TrackData = TrackData
  { event :: Text
  , properties :: Properties
  } deriving (Generic, ToJSON)

instance ToHttpApiData TrackData where
  toUrlPiece = toS . B64.encode . encode

mkTrackData :: AuthToken -> Text -> [Pair] -> TrackData
mkTrackData token event props =
  TrackData { event = event, properties = properties }
  where
    properties = mkProperties token (fromList props)

data Properties = Properties
  { token :: AuthToken
  , distinctId :: Maybe Text
  , time :: Maybe POSIXTime
  , ip :: Maybe Text
  , extraProperties :: Object
  }

instance ToJSON Properties where
  toJSON Properties{..} = Object $ extraProperties <> fromList
     [ "token" .= token
     , "distinct_id" .= distinctId
     , "time" .= time
     , "ip" .= ip
     ]

mkProperties :: AuthToken -> Object -> Properties
mkProperties token obj = Properties
  { token = token
  , distinctId = Nothing
  , time = Nothing
  , ip = Nothing
  , extraProperties = obj
  }
