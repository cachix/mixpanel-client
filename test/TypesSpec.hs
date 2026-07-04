module TypesSpec where

import Data.Aeson
import GHC.Exts   (fromList)
import Test.Hspec

import MixPanel.Types.Core
import MixPanel.Types.EngageData
import MixPanel.Types.ImportData
import MixPanel.Types.TrackData


spec :: Spec
spec = describe "TrackData/EngageData/ImportData" $ do
  it "TrackData merges properties" $
    let
     td = TrackData "foobar" $ mkProperties (AuthToken "token123") props
     props :: Object
     props = fromList [ "distinct_id" .= ("bar" :: String) ]
    in encode td `shouldBe` "{\"event\":\"foobar\",\"properties\":{\"distinct_id\":\"bar\",\"ip\":null,\"time\":null,\"token\":\"token123\"}}"
  it "EngageData merges properties" $
    let
      ed = mkEngageData (AuthToken "foobar") "123" $ Set (fromList [ "customProperty" .= ("foobar" :: String)])
    in encode ed `shouldBe` "{\"$distinct_id\":\"123\",\"$ignore_time\":null,\"$ip\":null,\"$set\":{\"customProperty\":\"foobar\"},\"$time\":null,\"$token\":\"foobar\"}"
  it "ImportEvent encodes required and extra properties" $
    let
      ie = mkImportEvent "Played Video" 1609459200 "user-1" "insert-1"
             [ "customProperty" .= True ]
    in encode ie `shouldBe` "{\"event\":\"Played Video\",\"properties\":{\"$insert_id\":\"insert-1\",\"customProperty\":true,\"distinct_id\":\"user-1\",\"time\":1609459200}}"
  it "ImportEvent typed fields win over overriding extras" $
    let
      ie = mkImportEvent "Played Video" 1609459200 "user-1" "insert-1"
             [ "time" .= (42 :: Integer)
             , "distinct_id" .= ("evil" :: String)
             , "$insert_id" .= ("evil-insert" :: String)
             , "customProperty" .= True
             ]
    in encode ie `shouldBe` "{\"event\":\"Played Video\",\"properties\":{\"$insert_id\":\"insert-1\",\"customProperty\":true,\"distinct_id\":\"user-1\",\"time\":1609459200}}"
  it "mkMergeEvent encodes to the $merge shape" $
    let
      me = mkMergeEvent "id1" "id2"
    in encode me `shouldBe` "{\"event\":\"$merge\",\"properties\":{\"$distinct_ids\":[\"id1\",\"id2\"]}}"
  it "mkIdentifiedEvent links the device id to the user id" $
    let
      ie = mkIdentifiedEvent "Signup" 1609459200 "user-1" "device-1" "insert-1" []
    in encode ie `shouldBe` "{\"event\":\"Signup\",\"properties\":{\"$device_id\":\"device-1\",\"$insert_id\":\"insert-1\",\"$user_id\":\"user-1\",\"distinct_id\":\"user-1\",\"time\":1609459200}}"
  it "ImportResponse parses a success body" $
    (decode "{\"code\":200,\"num_records_imported\":1,\"status\":\"OK\"}" :: Maybe ImportResponse)
      `shouldBe` Just ImportResponse
        { code = 200
        , status = "OK"
        , numRecordsImported = Just 1
        , importError = Nothing
        , failedRecords = []
        }
  it "ImportResponse parses a strict-mode validation error" $
    (decode "{\"code\":400,\"error\":\"validation failed\",\"status\":\"Bad Request\",\"num_records_imported\":1,\"failed_records\":[{\"index\":0,\"$insert_id\":\"insert-1\",\"field\":\"properties.time\",\"message\":\"bad time\"}]}" :: Maybe ImportResponse)
      `shouldBe` Just ImportResponse
        { code = 400
        , status = "Bad Request"
        , numRecordsImported = Just 1
        , importError = Just "validation failed"
        , failedRecords =
            [ FailedRecord
                { failedIndex = 0
                , failedInsertId = Just "insert-1"
                , failedField = Just "properties.time"
                , failedMessage = "bad time"
                }
            ]
        }
  it "FailedRecord decodes the $insert_id spelling" $
    (decode "{\"index\":0,\"$insert_id\":\"insert-1\",\"field\":\"properties.time\",\"message\":\"bad time\"}" :: Maybe FailedRecord)
      `shouldBe` Just FailedRecord
        { failedIndex = 0
        , failedInsertId = Just "insert-1"
        , failedField = Just "properties.time"
        , failedMessage = "bad time"
        }
  it "FailedRecord decodes the bare insert_id spelling" $
    (decode "{\"index\":0,\"insert_id\":\"insert-1\",\"field\":\"properties.time\",\"message\":\"bad time\"}" :: Maybe FailedRecord)
      `shouldBe` Just FailedRecord
        { failedIndex = 0
        , failedInsertId = Just "insert-1"
        , failedField = Just "properties.time"
        , failedMessage = "bad time"
        }
