module Line.Messaging.Webhook.ValidationSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import Crypto.Hash.SHA256 (hmaclazy)
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.Webhook.Validation (validateSignature)
import Network.Wai (Request(..), defaultRequest)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64

createRequest :: B.ByteString -> Request
createRequest signature = defaultRequest { requestHeaders = headers }
  where
    headers = [ ("X-Line-Signature", signature) ]

spec :: Spec
spec = do
  describe "validation" $ do
    it "returns true for valid signature" $ property $ \ channelSecret body ->
      let
        signature = Base64.encode $ hmaclazy (encodeUtf8 channelSecret) body
        request = createRequest signature
      in
        validateSignature channelSecret request body

    it "returns false for invalid signature" $ property $ \ channelSecret body ->
      let
        right = Base64.encode $ hmaclazy (encodeUtf8 channelSecret) body
        wrong = right `B.append` "wrong"
        request = createRequest wrong
      in
        not $ validateSignature channelSecret request body
