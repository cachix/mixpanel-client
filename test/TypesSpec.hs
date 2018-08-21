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
    in encode td `shouldBe` "{\"event\":\"foobar\",\"properties\":{\"time\":null,\"token\":\"token123\",\"ip\":null,\"distinct_id\":\"bar\"}}"
  it "EngageData merges properties" $
    let
      ed = mkEngageData (AuthToken "foobar") "123" $ Set (fromList [ "customProperty" .= ("foobar" :: String)])
    in encode ed `shouldBe` "{\"$distinct_id\":\"123\",\"$set\":{\"customProperty\":\"foobar\"},\"$token\":\"foobar\",\"$ip\":null,\"$ignoreTime\":null,\"$time\":null}"
