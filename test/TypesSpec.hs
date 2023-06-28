module TypesSpec where

import Data.Aeson
import GHC.Exts   (fromList)
import Test.Hspec

import MixPanel.Types.Core
import MixPanel.Types.EngageData
import MixPanel.Types.TrackData


spec :: Spec
spec = describe "TrackData/EngageData" $ do
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
