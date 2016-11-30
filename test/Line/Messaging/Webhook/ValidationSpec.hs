module Line.Messaging.Webhook.ValidationSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import Crypto.Hash.SHA256 (hmaclazy)
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.Webhook.Validation (validateSignature)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64

spec :: Spec
spec = do
  describe "validation" $ do
    it "returns true for valid signature" $ property $ \ channelSecret body ->
      let
        signature = Base64.encode $ hmaclazy (encodeUtf8 channelSecret) body
      in
        validateSignature channelSecret body signature

    it "returns false for invalid signature" $ property $ \ channelSecret body ->
      let
        right = Base64.encode $ hmaclazy (encodeUtf8 channelSecret) body
        wrong = right `B.append` "wrong"
      in
        not $ validateSignature channelSecret body wrong
