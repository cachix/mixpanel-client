module MixPanel
  ( runMixPanel
  , MixPanel
  , AuthToken(..)
  , DidSucceed(..)
  , Operation(..)
  , DistinctId
  , mkEnv
  , Env
  , track
  , alias
  , engage
  -- manager reexports
  , tlsManagerSettings
  , HTTP.newManager
  ) where

import           Control.Monad.Trans.Reader     ( ReaderT(..)
                                                , runReaderT
                                                , ask
                                                )
import           Control.Monad.Trans.Class      ( lift )
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

trackC :: TrackData -> Maybe Toggle -> Maybe Text -> Maybe Toggle -> Maybe Text -> Maybe Toggle -> ClientM DidSucceed
engageC :: EngageData -> Maybe Text -> Maybe Text -> Maybe Toggle -> ClientM DidSucceed
trackC :<|> engageC = client api

trackC' :: TrackData -> ClientM DidSucceed
trackC' trackdata = trackC trackdata Nothing Nothing Nothing Nothing (Just On)

engageC' :: EngageData -> ClientM DidSucceed
engageC' engagedata = engageC engagedata Nothing Nothing (Just On)

runMixPanel :: Env -> MixPanel a -> IO (Either ServantError a)
runMixPanel env comp = runClientM (runReaderT comp env) (clientEnv env)

track :: Text -> Object -> MixPanel DidSucceed
track event props = do
   Env{..} <- ask
   lift $ trackC' $ TrackData event $ mkProperties authtoken props

-- | Merges distinct id into alias id
alias :: DistinctId -> Text -> MixPanel DidSucceed
alias distinctId aliasId = do
  Env{..} <- ask
  lift $ trackC' $ TrackData "$create_alias" $ mkProperties authtoken props
    where
      props :: Object
      props = fromList [ "alias" .= aliasId
                       , "distinct_id" .= distinctId]

engage :: DistinctId -> Operation -> MixPanel DidSucceed
engage distinctid operation = do
  Env{..} <- ask
  lift $ engageC' $ mkEngageData authtoken distinctid operation
