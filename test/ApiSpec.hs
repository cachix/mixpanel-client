module ApiSpec where

import           Test.Hspec

import           MixPanel.API              ( importResponseFromError )
import           MixPanel.Types.ImportData


spec :: Spec
spec = describe "importResponseFromError" $ do
  it "recovers a strict-mode 400 validation body into an ImportResponse" $
    importResponseFromError 400
      "{\"code\":400,\"status\":\"Bad Request\",\"num_records_imported\":1}"
      `shouldBe` Just ImportResponse
        { code = 400
        , status = "Bad Request"
        , numRecordsImported = Just 1
        , importError = Nothing
        , failedRecords = []
        }
  it "rejects a 401 even when its body decodes as an ImportResponse" $
    importResponseFromError 401
      "{\"code\":401,\"error\":\"Invalid service account credentials\",\"status\":\"Unauthorized\"}"
      `shouldBe` Nothing
  it "rejects a 429 whose body shares the same shape" $
    importResponseFromError 429 "{\"code\":429,\"status\":\"Too Many Requests\"}"
      `shouldBe` Nothing
  it "rejects a 400 whose body cannot be decoded" $
    importResponseFromError 400 "not json" `shouldBe` Nothing
