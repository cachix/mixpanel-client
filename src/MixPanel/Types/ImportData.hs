module MixPanel.Types.ImportData
  ( ImportPayload(..)
  , ImportEvent(..)
  , ImportProperties(..)
  , mkImportEvent
  , mkMergeEvent
  , mkIdentifiedEvent
  , ImportResponse(..)
  , FailedRecord(..)
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , FromJSON
                                                , parseJSON
                                                , withObject
                                                , Value(..)
                                                , Object
                                                , (.=)
                                                , (.:)
                                                , (.:?)
                                                , (.!=)
                                                )
import           Data.Aeson.Types               ( Pair )
import           Data.Text                      ( Text )
import           Data.Time.Clock.POSIX          ( POSIXTime )
import           GHC.Exts                       ( fromList )

import           MixPanel.Types.EngageData      ( DistinctId )


-- | A single record to send to Mixpanel's @\/import@ endpoint.
--
-- @\/import@ accepts more than ordinary events, so the payload is a sum:
--
-- * 'Event' — an ordinary tracked event with the required typed properties.
-- * 'Merge' — a @$merge@ identity record joining two @distinct_id@s.
data ImportPayload
  = Event ImportEvent
  | Merge DistinctId DistinctId

instance ToJSON ImportPayload where
  toJSON (Event importEvent) = toJSON importEvent
  toJSON (Merge idA idB) = Object $ fromList
    [ "event" .= ("$merge" :: Text)
    , "properties" .= Object (fromList [ "$distinct_ids" .= [idA, idB] ])
    ]

-- | A single event to send to Mixpanel's @\/import@ endpoint.
--
-- Unlike the @\/track@ payload, the project token is not part of the
-- event; authentication happens at the request level (see
-- 'MixPanel.Auth.ProjectToken' and 'MixPanel.Auth.ServiceAccount').
data ImportEvent = ImportEvent
  { event :: Text
  , properties :: ImportProperties
  }

instance ToJSON ImportEvent where
  toJSON ImportEvent{..} = Object $ fromList
    [ "event" .= event
    , "properties" .= properties
    ]

-- | Properties of an 'ImportEvent'. @\/import@ requires @time@, @distinct_id@
-- and @$insert_id@ on every event.
data ImportProperties = ImportProperties
  { time :: POSIXTime
  , distinctId :: DistinctId
  , insertId :: Text
  , extraProperties :: Object
  }

instance ToJSON ImportProperties where
  toJSON ImportProperties{..} = Object $ fromList
    -- strict mode requires the timestamp as (integer) seconds since epoch.
    -- The typed fields are listed first so that, since aeson's 'KeyMap'
    -- '<>' is left-biased, they win over any same-named keys in the extras.
    [ "time" .= (round time :: Integer)
    , "distinct_id" .= distinctId
    , "$insert_id" .= insertId
    ] <> extraProperties

-- | Build an ordinary event payload from its required fields and a list of
-- extra properties.
--
-- The @$insert_id@ is used by Mixpanel for deduplication and must be supplied
-- by the caller: use an existing unique event id, or the first 36 characters
-- of a hash of properties that make the event semantically unique.
--
-- The reserved keys @time@, @distinct_id@ and @$insert_id@ in the extra
-- properties are ignored in favour of the corresponding typed arguments.
mkImportEvent :: Text -> POSIXTime -> DistinctId -> Text -> [Pair] -> ImportPayload
mkImportEvent event time distinctId insertId props =
  Event ImportEvent { event = event, properties = properties }
  where
    properties = ImportProperties
      { time = time
      , distinctId = distinctId
      , insertId = insertId
      , extraProperties = fromList props
      }

-- | Build a @$merge@ identity record for the @\/import@ endpoint.
--
-- An identity merge tells Mixpanel that two @distinct_id@s refer to the same
-- user, combining their event and profile history into a single identity
-- cluster. The record carries only the two ids to merge — none of the usual
-- @time@ \/ @distinct_id@ \/ @$insert_id@ properties apply.
--
-- __Only valid on projects using the original ID merge API.__ Projects on
-- the simplified ID merge system silently ignore @$merge@ records — they do
-- not show up in 'failedRecords' — so sending one there fails without any
-- feedback. For simplified ID merge projects, link identities with
-- 'mkIdentifiedEvent' instead.
mkMergeEvent :: DistinctId -> DistinctId -> ImportPayload
mkMergeEvent = Merge

-- | Build an ordinary event that also links a device id to a user id, for
-- projects on the __simplified ID merge__ system.
--
-- The first time Mixpanel sees a @$device_id@ and a @$user_id@ together on
-- one event, it merges the two into a single identity cluster. The event's
-- @distinct_id@ is set to the user id, matching what Mixpanel computes
-- server-side. The @time@ \/ @$insert_id@ \/ extra-properties arguments
-- behave as in 'mkImportEvent'.
mkIdentifiedEvent
  :: Text        -- ^ event name
  -> POSIXTime
  -> DistinctId  -- ^ @$user_id@ — also becomes the event's @distinct_id@
  -> Text        -- ^ @$device_id@
  -> Text        -- ^ @$insert_id@
  -> [Pair]
  -> ImportPayload
mkIdentifiedEvent event time userId deviceId insertId props =
  -- 'fromList' is later-wins, so listing the linking ids after the caller's
  -- extras keeps the typed-arguments-win convention of 'mkImportEvent'.
  mkImportEvent event time userId insertId $
    props <>
      [ "$user_id" .= userId
      , "$device_id" .= deviceId
      ]

-- | The response returned by the @\/import@ endpoint (both the @200@ success
-- and the strict-mode @400@ validation-error bodies parse into this type).
--
-- A non-empty 'failedRecords' signals a partial failure.
data ImportResponse = ImportResponse
  { code :: Int
  , status :: Text
  , numRecordsImported :: Maybe Int
  , importError :: Maybe Text
  , failedRecords :: [FailedRecord]
  } deriving (Show, Eq)

instance FromJSON ImportResponse where
  parseJSON = withObject "ImportResponse" $ \o ->
    ImportResponse
      <$> o .:  "code"
      <*> o .:  "status"
      <*> o .:? "num_records_imported"
      <*> o .:? "error"
      <*> o .:? "failed_records" .!= []

-- | A single event that failed validation in strict mode.
data FailedRecord = FailedRecord
  { failedIndex :: Int
  , failedInsertId :: Maybe Text
  , failedField :: Maybe Text
  , failedMessage :: Text
  } deriving (Show, Eq)

instance FromJSON FailedRecord where
  parseJSON = withObject "FailedRecord" $ \o ->
    FailedRecord
      <$> o .:  "index"
      -- Mixpanel's failed-record examples spell the key both ways; prefer the
      -- documented @$insert_id@ and fall back to the bare @insert_id@.
      <*> ((Just <$> o .: "$insert_id") <|> o .:? "insert_id")
      <*> o .:? "field"
      <*> o .:  "message"
