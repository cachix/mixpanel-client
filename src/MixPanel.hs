module MixPanel
  ( MixPanelError(..)
  , AuthToken(..)
  , Operation(..)
  , DistinctId
  -- Setup
  , mkEnv
  , Env
  -- mixpanel api calls
  , track
  , alias
  , engage
  -- HTTP manager reexports
  , tlsManagerSettings
  , HTTP.newManager
  ) where

import           Control.Exception              ( Exception )
import           Data.Aeson                     ( Object, (.=) )
import           Data.Text                      ( Text )
import           GHC.Exts                       ( fromList )
import qualified Network.HTTP.Client           as HTTP
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Servant.API             hiding ( URI )
import           Servant.Client

import           MixPanel.API                   ( api )
import           MixPanel.Types.Core            ( DidSucceed(..)
                                                , Toggle(..)
                                                , AuthToken(..)
                                                )
import           MixPanel.Types.TrackData       ( TrackData(..)
                                                , mkProperties
                                                )
import           MixPanel.Types.EngageData      ( EngageData, DistinctId, Operation(..), mkEngageData )


host :: BaseUrl
host = BaseUrl Https "api.mixpanel.com" 443 ""

data MixPanelError
  = ServantError ServantError
  | Error Text
  deriving (Show, Exception)

data Env = Env
  { authtoken :: AuthToken
  , httpManager :: HTTP.Manager
  , clientEnv :: ClientEnv
  }

mkEnv :: AuthToken -> HTTP.Manager -> Env
mkEnv authtoken httpManager = Env {..}
  where
    clientEnv = mkClientEnv httpManager host



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
    Left err -> Left $ ServantError err
    Right (Fail err) -> Left $ Error err
    Right Success -> Right ()

track :: Env -> Text -> Object -> IO (Either MixPanelError ())
track Env{..} event props =
  runMixPanel clientEnv $ trackC' $ TrackData event $ mkProperties authtoken props

-- | Renames distinct id into alias id
alias :: Env -> DistinctId -> Text -> IO (Either MixPanelError ())
alias Env{..} distinctId aliasId =
   runMixPanel clientEnv $ trackC' $ TrackData "$create_alias" $ mkProperties authtoken props
    where
      props :: Object
      props = fromList [ "alias" .= aliasId
                       , "distinct_id" .= distinctId]

engage :: Env -> DistinctId -> Operation -> IO (Either MixPanelError ())
engage Env{..} distinctid operation =
  runMixPanel clientEnv $ engageC' $ mkEngageData authtoken distinctid operation
