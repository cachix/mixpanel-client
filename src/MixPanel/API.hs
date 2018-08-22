{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{- As per https://mixpanel.com/help/reference/http
-}
module MixPanel.API
  ( API
  , api
  ) where

import           Data.Text                      ( Text )
import           Data.Proxy                     ( Proxy(..) )
import           Servant.API             hiding ( URI )

import           MixPanel.Types.Core            ( DidSucceed, Toggle )
import           MixPanel.Types.TrackData       ( TrackData )
import           MixPanel.Types.EngageData      ( EngageData )


type Track = "track"
          :> QueryParam' '[Required] "data" TrackData
          :> QueryParam "ip" Toggle
          :> QueryParam "redirect" Text -- URI
          :> QueryParam "img" Toggle
          :> QueryParam "callback" Text
          :> QueryParam "verbose" Toggle
          :> Get '[JSON] DidSucceed

type Engage = "engage"
           :> QueryParam' '[Required] "data" EngageData
           :> QueryParam "redirect" Text -- URI
           :> QueryParam "callback" Text
           :> QueryParam "verbose" Toggle
           :> Get '[JSON] DidSucceed

type API = Track :<|> Engage

api :: Proxy API
api = Proxy
