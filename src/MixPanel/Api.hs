{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{- As per https://mixpanel.com/help/reference/http
-}
module MixPanel.Api
  ( API
  , api
  ) where


import Data.Text (Text)

import Data.Proxy (Proxy(..))
import Servant.API hiding (URI)

import MixPanel.Types.Core (IsSuccess)
import MixPanel.Types.TrackData (TrackData)
import MixPanel.Types.EngageData (EngageData)


type Track = "track"
          :> QueryParam' '[Required] "data" TrackData
          :> QueryParam "ip" IsSuccess
          :> QueryParam "redirect" Text -- URI
          :> QueryParam "img" IsSuccess
          :> QueryParam "callback" Text
          :> QueryParam "verbose" IsSuccess
          :> Get '[JSON] IsSuccess

type Engage = "engage"
           :> QueryParam' '[Required] "data" EngageData
           :> QueryParam "redirect" Text -- URI
           :> QueryParam "callback" Text
           :> QueryParam "verbose" IsSuccess
           :> Get '[JSON] IsSuccess

type API = Track :<|> Engage

api :: Proxy API
api = Proxy

{- TODO:
- engage special properties
- batch requests
- tracking revenue
- import/export
-}
