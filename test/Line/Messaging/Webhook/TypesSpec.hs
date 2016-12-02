module Line.Messaging.Webhook.TypesSpec where

import Test.Hspec

import Line.Messaging.Webhook.TypesSpecHelper

import Data.Aeson
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Line.Messaging.Webhook.Types
import qualified Data.ByteString.Lazy as BL

fromJSONSpec :: (FromJSON a, Eq a, Show a)
             => [(BL.ByteString, Maybe a)]
             -> SpecWith ()
fromJSONSpec ((raw, result):xs) = do
  let title = if isJust result then "success" else "fail"
  it title $ decode raw `shouldBe` result
  fromJSONSpec xs
fromJSONSpec [] = return ()

datetime :: Integer -> UTCTime
datetime = posixSecondsToUTCTime . (/ 1000) . fromInteger

spec :: Spec
spec = do
  describe "parse body" $ fromJSONSpec
    [ ( goodBody, Just $ Body [] )
    , ( badBody, Nothing)
    ]

  describe "parse beacon event" $ fromJSONSpec
    [ ( goodBeacon, Just $ Body $ [
          BeaconEvent ( User "U206d25c2ea6bd87c17655609a1c37cb8"
                     , datetime 1462629479859
                     , "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA"
                     , BeaconEnter "d41d8cd98f"
                     )
          ])
    , ( badBeacon, Nothing)
    ]
