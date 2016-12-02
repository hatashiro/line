{-# LANGUAGE QuasiQuotes #-}

module Line.Messaging.Webhook.TypesSpecHelper where

import Text.RawString.QQ
import qualified Data.ByteString.Lazy as BL

goodBody :: BL.ByteString
goodBody = [r|
{ "events": [] }
|]

badBody :: BL.ByteString
badBody = [r|
{ "event": [] }
|]

goodBeacon :: BL.ByteString
goodBeacon = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "beacon",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "beacon": {
    "hwid": "d41d8cd98f",
    "type": "enter"
  }
}
] }
|]

badBeacon :: BL.ByteString
badBeacon = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "beacon",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "bacon": {
    "hwid": "d41d8cd98f",
    "type": "enter"
  }
}
] }
|]
