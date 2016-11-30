module Line.Messaging.WebhookSpec where

import Test.Hspec
import Test.Hspec.Wai

import Control.Monad.Trans.Except (runExceptT)
import Crypto.Hash.SHA256 (hmaclazy)
import Data.Either (isRight)
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.Webhook
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64

emptyBody :: BL.ByteString
emptyBody = "{ \"events\": [] }"

wrongBody :: BL.ByteString
wrongBody = "{ \"event\": [] }"

createSig :: ChannelSecret -> BL.ByteString -> Signature
createSig sec body = Base64.encode $ hmaclazy (encodeUtf8 sec) body

spec :: Spec
spec = do
  describe "Basic webhook" $ do
    it "right result" $ do
      let secret = "some-secret"
          sig = createSig secret emptyBody
      result <- runExceptT $ webhook secret emptyBody sig
      result `shouldSatisfy` isRight

    it "wrong sig" $ do
      let secret = "some-secret"
          sig = "wrong-sig"
      result <- runExceptT $ webhook secret emptyBody sig
      result `shouldBe` Left SignatureVerificationFailed

    it "malformed body" $ do
      let secret = "some-secret"
          sig = createSig secret wrongBody
      result <- runExceptT $ webhook secret wrongBody sig
      result `shouldBe` Left MessageDecodeFailed

  describe "WAI webhook" $
    let waiApp = return $ webhookApp "some-secret" (const $ return Ok) defaultOnFailure
        webhookReq sec body hasCorrectSig = request "GET" "/" headers body
          where
            sig = if hasCorrectSig then createSig sec body else "wrong sig"
            headers = [ ("X-Line-Signature", sig) ]
    in with waiApp $ do
      it "handle req well" $ do
        webhookReq "some-secret" emptyBody True `shouldRespondWith` 200

      it "with wrong sig" $ do
        webhookReq "some-secret" emptyBody False `shouldRespondWith`
          "SignatureVerificationFailed" { matchStatus = 400 }

      it "malformed body" $ do
        webhookReq "some-secret" wrongBody True `shouldRespondWith`
          "MessageDecodeFailed" { matchStatus = 400 }
