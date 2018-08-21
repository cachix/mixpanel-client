module MixPanel
  ( runMixPanel
  , MixPanel
  , AuthToken(..)
  , IsSuccess(..)
  , mkEnv
  , track
  , alias
  -- manager reexports
  , tlsManagerSettings
  , HTTP.newManager
  ) where

import           Control.Monad.Trans.Reader     ( ReaderT(..)
                                                , runReaderT
                                                , ask
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Data.Aeson                     ( ToJSON, object, Object, (.=) )
import           Data.Text                      ( Text )
import qualified Network.HTTP.Client           as HTTP
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Servant.API             hiding ( URI )
import           Servant.Client

import           MixPanel.Api                   ( api )
import           MixPanel.Types.Core            ( IsSuccess(..)
                                                , AuthToken(..)
                                                )
import           MixPanel.Types.TrackData       ( TrackData(..)
                                                , mkProperties
                                                )
import           MixPanel.Types.EngageData      ( EngageData )


host :: BaseUrl
host = BaseUrl Https "api.mixpanel.com" 443 ""

data Env = Env
  { authtoken :: AuthToken
  , httpManager :: HTTP.Manager
  , clientEnv :: ClientEnv
  }

mkEnv :: AuthToken -> HTTP.Manager -> Env
mkEnv authtoken httpManager = Env {..}
  where
    clientEnv = mkClientEnv httpManager host

type MixPanel = ReaderT Env ClientM

trackC :: TrackData -> Maybe IsSuccess -> Maybe Text -> Maybe IsSuccess -> Maybe Text -> Maybe IsSuccess -> ClientM IsSuccess
engageC ::  EngageData -> Maybe Text -> Maybe Text -> Maybe IsSuccess -> ClientM IsSuccess
trackC :<|> engageC = client api

trackC' :: TrackData -> ClientM IsSuccess
trackC' trackdata = trackC trackdata Nothing Nothing Nothing Nothing Nothing

runMixPanel :: Env -> MixPanel a -> IO (Either ServantError a)
runMixPanel env comp = runClientM (runReaderT comp env) (clientEnv env)

track :: Text -> Object -> MixPanel IsSuccess
track event props = do
   Env{..} <- ask
   lift $ trackC' $ TrackData event $ mkProperties authtoken props

-- | Merges distinct id into alias id
alias :: Text -> Text -> MixPanel IsSuccess
alias distinctId aliasId = do
  Env{..} <- ask
  lift $ trackC' $ TrackData "$create_alias" $ mkProperties authtoken props
    where
      props :: Object
      props = mempty
