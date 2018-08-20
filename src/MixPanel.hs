module MixPanel
  ( runMixPanel
  , MixPanel
  , Token(..)
  , mkEnv
  , track
  -- manager reexports
  , tlsManagerSettings
  , HTTP.newManager
  ) where

-- TODO: alias with distinct_id: https://github.com/mixpanel/mixpanel-python/blob/master/mixpanel/__init__.py#L150
-- TODO: mixpanel datetime
-- TODO: verbose by default

import           Control.Monad.Trans.Reader     ( ReaderT(..), runReaderT )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import Data.Aeson      (ToJSON)
import           Data.Text                      ( Text )
import qualified Network.HTTP.Client           as HTTP
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Servant.API             hiding ( URI )
import           Servant.Client
import           Text.URI                       ( URI )

import           MixPanel.Api                   ( api
                                                , API
                                                )
import           MixPanel.Types.Core            ( IsSuccess
                                                , Token(..)
                                                )
import           MixPanel.Types.TrackData       ( TrackData(..), mkProperties )
import           MixPanel.Types.EngageData      ( EngageData )


host :: BaseUrl
host = BaseUrl Https "api.mixpanel.com" 443 ""

data Env = Env
  { token :: Token
  , httpManager :: HTTP.Manager
  , clientEnv :: ClientEnv
  }

mkEnv :: Token -> HTTP.Manager -> Env
mkEnv token httpManager = Env {..}
  where
    clientEnv = mkClientEnv httpManager host

type MixPanel = ReaderT Env ClientM


trackC :: TrackData -> Maybe IsSuccess -> Maybe Text -> Maybe IsSuccess -> Maybe Text -> Maybe IsSuccess -> ClientM IsSuccess
engageC ::  EngageData -> Maybe Text -> Maybe Text -> Maybe IsSuccess -> ClientM IsSuccess
trackC :<|> engageC = client api

runMixPanel :: Env -> MixPanel a -> IO (Either ServantError a)
runMixPanel env comp = runClientM (runReaderT comp env) (clientEnv env)

track :: ToJSON a => Text -> Maybe a -> MixPanel IsSuccess
track event props = ReaderT $ \Env{..} ->
   trackC (TrackData event (mkProperties token props)) Nothing Nothing Nothing Nothing Nothing
