{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module MixPanel.Types.EngageData
  ( EngageData(..)
  , DistinctId
  , mkEngageData
  , Operation(..)
  ) where

import           Data.Text                      ( Text )
import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , Object
                                                , Value(..)
                                                , Array
                                                , encode
                                                , (.=)
                                                )
import qualified Data.ByteString.Base64.Lazy   as B64
import           Data.String.Conv               ( toS )
import           GHC.Exts                       ( fromList )
import           Servant.API

import           MixPanel.Types.Core            ( AuthToken )


data Operation where
  Set :: Object -> Operation
  SetOnce :: Object -> Operation
  Add :: Object -> Operation
  Append :: Object -> Operation
  Union :: Object -> Operation
  Remove :: Object -> Operation
  Unset :: Array -> Operation
  Delete :: Operation

instance ToJSON Operation where
  toJSON (Set obj) = Object obj
  toJSON (SetOnce obj) = Object obj
  toJSON (Add obj) = Object obj
  toJSON (Append obj) = Object obj
  toJSON (Union obj) = Object obj
  toJSON (Remove obj) = Object obj
  toJSON (Unset obj) = Array obj
  toJSON Delete = ""

operationIdentifier :: Operation -> Text
operationIdentifier (Set _) = "$set"
operationIdentifier (SetOnce _) = "$set_once"
operationIdentifier (Add _) = "$add"
operationIdentifier (Append _) = "$append"
operationIdentifier (Union _) = "$union"
operationIdentifier (Remove _) = "$remove"
operationIdentifier (Unset _) = "$unset"
operationIdentifier Delete = "$delete"

type DistinctId = Text

data EngageData = EngageData
  { token :: AuthToken
  , distinctId :: DistinctId
  , ip :: Maybe Text
  , time :: Maybe Text
  , ignoreTime :: Maybe Text
  , operation :: Operation
  }

instance ToJSON EngageData where
  toJSON EngageData{..} = Object $ fromList
    [ "$token" .= token
    , "$distinct_id" .= distinctId
    , "$time" .= time
    , "$ignoreTime" .= ignoreTime
    , "$ip" .= ip
    , operationIdentifier operation .= toJSON operation
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
