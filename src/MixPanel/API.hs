{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{- As per https://mixpanel.com/help/reference/http
-}
module MixPanel.API
  ( -- * Servant API
    API
  , api

  -- * Servant client functions
  , engageC
  , trackC

  -- * Haskell API
  , alias
  , engage
  , engage'
  , track

  -- * Run the Servant clients
  , runMixPanel
  , MixPanelError(..)
  ) where

import           Control.Exception              ( Exception )
import           Data.Aeson                     ( Object, (.=) )
import           Data.Text                      ( Text )
import           GHC.Exts                       ( fromList )
import           Data.Proxy                     ( Proxy(..) )
import           Servant.API             hiding ( URI )
import           Servant.Client                 ( client, ClientEnv, ClientError, ClientM, runClientM )
import           MixPanel.Env                   ( Env(..) )
import           MixPanel.Types.Core            ( DidSucceed(..)
                                                , Toggle(..)
                                                )
import           MixPanel.Types.TrackData       ( TrackData(..)
                                                , mkProperties
                                                )
import           MixPanel.Types.EngageData      ( EngageData, DistinctId, Operation(..), mkEngageData )


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

data MixPanelError
  = ClientError ClientError
  | Error Text
  deriving (Show, Exception)

#if !MIN_VERSION_servant_client(0,16,0)
#define ClientError ServantError
#endif

trackC :: TrackData -> Maybe Toggle -> Maybe Text -> Maybe Toggle -> Maybe Text -> Maybe Toggle -> ClientM DidSucceed
engageC :: EngageData -> Maybe Text -> Maybe Text -> Maybe Toggle -> ClientM DidSucceed
trackC :<|> engageC = client api

trackC' :: TrackData -> ClientM DidSucceed
trackC' trackdata = trackC trackdata Nothing Nothing Nothing Nothing (Just On)

engageC' :: EngageData -> ClientM DidSucceed
engageC' engagedata =
  engageC engagedata Nothing Nothing (Just On)

runMixPanel :: ClientEnv -> ClientM DidSucceed -> IO (Either MixPanelError ())
runMixPanel clientEnv comp = do
  result <- runClientM comp clientEnv
  return $ case result of
    Left err -> Left $ ClientError err
    Right (Fail err) -> Left $ Error err
    Right Success -> Right ()

-- | Renames distinct id into alias id
alias :: Env -> DistinctId -> Text -> IO (Either MixPanelError ())
alias Env{..} distinctId aliasId =
   runMixPanel clientEnv $ trackC' $ TrackData "$create_alias" $ mkProperties authtoken props
    where
      props :: Object
      props = fromList [ "alias" .= aliasId
                       , "distinct_id" .= distinctId]

track :: Env -> Text -> Object -> IO (Either MixPanelError ())
track Env{..} event props =
  runMixPanel clientEnv $ trackC' $ TrackData event $ mkProperties authtoken props

engage :: Env -> DistinctId -> Operation -> IO (Either MixPanelError ())
engage Env{..} distinctid operation =
  runMixPanel clientEnv $ engageC' $ mkEngageData authtoken distinctid operation

engage' :: Env -> DistinctId -> (EngageData -> EngageData) -> Operation -> IO (Either MixPanelError ())
engage' Env{..} distinctid f operation =
  runMixPanel clientEnv $ engageC' $ f $ mkEngageData authtoken distinctid operation

