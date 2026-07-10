module MixPanel.Auth
  ( Credentials(..)
  , ProjectToken(..)
  , ServiceAccount(..)
  , Auth(..)
  ) where

import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Servant.API                    ( BasicAuthData(..) )
import           MixPanel.Types.Core            ( AuthToken(..) )

-- | The wire-level material every authenticated request needs:
-- HTTP Basic auth plus an optional project id. Servant takes care of the
-- base64 encoding of the Basic credentials.
--
-- No 'Show' instance is derived on purpose; 'Credentials' carries secrets and
-- we would rather not leak them into logs.
data Credentials = Credentials
  { credBasicAuth :: BasicAuthData
  , credProjectId :: Maybe Text
  }

-- | Project-token auth: the token is the Basic auth username, the password is
-- empty. The project id is optional.
--
-- No 'Show' instance is derived on purpose; the token is a secret.
data ProjectToken = ProjectToken
  { ptToken :: AuthToken
  , ptProjectId :: Maybe Text
  }

-- | Service account auth — the method Mixpanel recommends for server-side
-- ingestion. The project id is required.
--
-- No 'Show' instance is derived on purpose; the secret must not leak.
data ServiceAccount = ServiceAccount
  { saUsername :: Text
  , saSecret :: Text
  , saProjectId :: Text
  }

-- | How to turn a credential into wire-level material. The signing logic for
-- each credential type is defined exactly once, here.
class Auth a where
  toCredentials :: a -> Credentials

instance Auth ProjectToken where
  toCredentials (ProjectToken (AuthToken token) projectId) =
    Credentials (BasicAuthData (encodeUtf8 token) "") projectId

instance Auth ServiceAccount where
  toCredentials (ServiceAccount user secret projectId) =
    Credentials (BasicAuthData (encodeUtf8 user) (encodeUtf8 secret)) (Just projectId)
