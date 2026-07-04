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
  , importC

  -- * Haskell API
  , alias
  , engage
  , engage'
  , track
  , importEvents
  , ImportAuth
  , ServiceAuth(..)

  -- * Run the Servant clients
  , runMixPanel
  , runImport
  , recoverImportError
  , importResponseFromError
  , MixPanelError(..)
  ) where

import           Control.Exception              ( Exception )
import           Data.Aeson                     ( Object, decode, (.=) )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Text                      ( Text )
import           GHC.Exts                       ( fromList )
import           Data.Proxy                     ( Proxy(..) )
import           Servant.API             hiding ( URI )
import           Servant.Client                 ( client, ClientEnv, ClientError(..), ClientM, runClientM, responseBody, responseStatusCode )
import           Network.HTTP.Types             ( statusCode )
import           MixPanel.Auth                  ( Auth(..)
                                                , Credentials(..)
                                                , ProjectToken
                                                , ServiceAccount(..)
                                                )
import           MixPanel.Env                   ( Env(..) )
import           MixPanel.Types.Core            ( DidSucceed(..)
                                                , Toggle(..)
                                                )
import           MixPanel.Types.TrackData       ( TrackData(..)
                                                , mkProperties
                                                )
import           MixPanel.Types.EngageData      ( EngageData, DistinctId, Operation(..), mkEngageData )
import           MixPanel.Types.ImportData      ( ImportPayload, ImportResponse )


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

-- | The @\/import@ ingestion endpoint. Sends a JSON array of payloads
-- (up to 2000 per request) authenticated with HTTP Basic auth.
type Import = "import"
           :> QueryParam "project_id" Text
           :> QueryParam' '[Required] "strict" Toggle
           :> BasicAuth "mixpanel" ()
           :> ReqBody '[JSON] [ImportPayload]
           :> Post '[JSON] ImportResponse

type API = Track :<|> Engage :<|> Import

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
importC :: Maybe Text -> Toggle -> BasicAuthData -> [ImportPayload] -> ClientM ImportResponse
trackC :<|> engageC :<|> importC = client api

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

-- | Run an @\/import@ client. In strict mode a validation failure comes back as
-- an HTTP @400@ whose body still describes the result; that body — and only
-- that body — is recovered into an 'ImportResponse' so the caller can inspect
-- 'MixPanel.Types.ImportData.failedRecords'. Every other failure is surfaced as
-- a 'ClientError'. See 'recoverImportError' for the exact decision.
runImport :: ClientEnv -> ClientM ImportResponse -> IO (Either MixPanelError ImportResponse)
runImport clientEnv comp = do
  result <- runClientM comp clientEnv
  return $ case result of
    Right resp -> Right resp
    Left err   -> recoverImportError err

-- | Interpret a Servant 'ClientError' from an @\/import@ call.
--
-- Only a strict-mode validation failure — an HTTP @400@ whose body still
-- describes the result — is recovered into a 'Right' 'ImportResponse'. Auth
-- failures (@401@), rate limits (@429@) and @5xx@ responses share the same
-- @code@\/@status@ body shape and would otherwise decode as a bogus success, so
-- any non-@400@ status is surfaced as a 'ClientError'. A @400@ whose body fails
-- to decode is likewise surfaced as a 'ClientError'.
recoverImportError :: ClientError -> Either MixPanelError ImportResponse
recoverImportError (FailureResponse _ response)
  | Just resp <- importResponseFromError statusC body = Right resp
  where
    statusC = statusCode (responseStatusCode response)
    body    = responseBody response
recoverImportError err = Left $ ClientError err

-- | The pure recovery decision, keyed on the HTTP status code and raw response
-- body: only a strict-mode @400@ whose body decodes yields an 'ImportResponse';
-- every other status (@401@, @429@, @5xx@, …) yields 'Nothing' even if its body
-- happens to have the same shape.
importResponseFromError :: Int -> ByteString -> Maybe ImportResponse
importResponseFromError statusC body
  | statusC == 400 = decode body
  | otherwise      = Nothing

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

-- | Credentials accepted by the ingestion endpoints (@\/import@).
-- The instance list is the capability matrix: both auth methods are accepted.
class Auth a => ImportAuth a
instance ImportAuth ProjectToken
instance ImportAuth ServiceAccount

-- | Credentials accepted by endpoints that require a service account and a
-- project id (query\/export\/GDPR-style APIs). Deliberately no instance for
-- 'ProjectToken' — Mixpanel rejects it there, so we reject it at compile time.
class Auth a => ServiceAuth a where
  serviceProjectId :: a -> Text

instance ServiceAuth ServiceAccount where
  serviceProjectId = saProjectId

-- | Import a batch of payloads — ordinary events or @$merge@ records — via
-- the @\/import@ endpoint.
--
-- Authenticate with either a 'MixPanel.Auth.ServiceAccount' or a
-- 'MixPanel.Auth.ProjectToken'; both credential types are accepted here.
--
-- Mixpanel accepts up to 2000 records (10 MB) per request; callers are
-- responsible for chunking larger batches. Uses strict mode so that invalid
-- records are reported in the response's 'MixPanel.Types.ImportData.failedRecords'.
importEvents :: ImportAuth a => Env -> a -> [ImportPayload] -> IO (Either MixPanelError ImportResponse)
importEvents Env{..} auth events =
  runImport clientEnv $ importC (credProjectId creds) On (credBasicAuth creds) events
  where
    creds = toCredentials auth

