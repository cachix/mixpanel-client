{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module MixPanel.Types.EngageData
  ( EngageData
  , mkEngageData
  , Operation(..)
  ) where

import           Data.ByteString                ( ByteString )
import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , object
                                                , encode
                                                )
import qualified Data.ByteString.Base64.Lazy   as B64
import           Data.String.Conv               ( toS )
import           Servant.API

import           MixPanel.Types.Core            ( AuthToken )


data Operation where
    Set :: ToJSON a => a -> Operation
    SetOnce :: ToJSON a => a -> Operation
    Add :: ToJSON a => a -> Operation
    Append :: ToJSON a => a -> Operation
    Union :: ToJSON a => a -> Operation
    Remove :: ToJSON a => a -> Operation
    Unset :: ToJSON [a] => [a] -> Operation
    Delete :: Operation


type DistinctId = ByteString

data EngageData = EngageData
  { token :: AuthToken
  , distinctId :: DistinctId
  , ip :: Maybe ByteString
  , time :: Maybe ByteString
  , ignoreTime :: Maybe ByteString
  , operation :: Operation
  }

-- TODO: properly set Operation
instance ToJSON EngageData where
  toJSON EngageData{..} = object
    [
    ]

instance ToHttpApiData EngageData where
  toUrlPiece = toS . B64.encode . encode

mkEngageData :: AuthToken -> DistinctId -> Operation -> EngageData
mkEngageData token di op = EngageData
  { token = token
  , distinctId = di
  , ip = Nothing
  , time = Nothing
  , ignoreTime = Nothing
  , operation = op
  }
