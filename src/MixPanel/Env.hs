module MixPanel.Env 
  ( Env(..)
  , mkEnv
  ) where

import           Servant.Client                 ( BaseUrl(..), ClientEnv, mkClientEnv, Scheme(..) )
import           MixPanel.Types.Core            ( AuthToken )
import qualified Network.HTTP.Client           as HTTP

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

