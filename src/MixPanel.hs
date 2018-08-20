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

import           Control.Monad.Trans.Reader     ( ReaderT(..), runReaderT )
import Data.Aeson      (ToJSON)
import           Data.Text                      ( Text )
import qualified Network.HTTP.Client           as HTTP
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Servant.API             hiding ( URI )
import           Servant.Client

import           MixPanel.Api                   ( api
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
