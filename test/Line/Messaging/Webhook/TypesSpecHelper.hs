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

goodUser :: BL.ByteString
goodUser = [r|
{ "type": "user", "userId": "123" }
|]

goodGroup :: BL.ByteString
goodGroup = [r|
{ "type": "group", "groupId": "456" }
|]

goodRoom :: BL.ByteString
goodRoom = [r|
{ "type": "room", "roomId": "789" }
|]

badSource :: BL.ByteString
badSource = [r|
{ "type": "bad", "userId": "123 }
|]

goodTextMessage :: BL.ByteString
goodTextMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "text",
    "text": "Hello, world"
  }
}
] }
|]

badTextMessage :: BL.ByteString
badTextMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "text'",
    "text": "Hello, world"
  }
}
] }
|]

goodImageMessage :: BL.ByteString
goodImageMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "image"
  }
}

] }
|]

badImageMessage :: BL.ByteString
badImageMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "image'"
  }
}
] }
|]

goodVideoMessage :: BL.ByteString
goodVideoMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "video"
  }
}
] }
|]

badVideoMessage :: BL.ByteString
badVideoMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "video'"
  }
}
] }
|]

goodAudioMessage :: BL.ByteString
goodAudioMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "audio"
  }
}
] }
|]

badAudioMessage :: BL.ByteString
badAudioMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "audio'"
  }
}
] }
|]

goodFileMessage :: BL.ByteString
goodFileMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "file",
    "fileName": "hello.txt",
    "fileSize": "1234"
  }
}
] }
|]

badFileMessage :: BL.ByteString
badFileMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "file'",
    "fileName": "hello.txt",
    "fileSize": "1234"
  }
}
] }
|]

goodLocationMessage :: BL.ByteString
goodLocationMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "location",
    "title": "my location",
    "address": "some address",
    "latitude": 35.65910807942215,
    "longitude": 139.70372892916203
  }
}
] }
|]

badLocationMessage :: BL.ByteString
badLocationMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id'": "325708",
    "type": "location",
    "title": "my location",
    "address": "〒150-0002 東京都渋谷区渋谷２丁目２１−１",
    "latitude": 35.65910807942215,
    "longitude": 139.70372892916203
  }
}
] }
|]

goodStickerMessage :: BL.ByteString
goodStickerMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message": {
    "id": "325708",
    "type": "sticker",
    "packageId": "1",
    "stickerId": "1"
  }
}
] }
|]

badStickerMessage :: BL.ByteString
badStickerMessage = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "message'": {
    "id": "325708",
    "type": "sticker",
    "packageId": "1",
    "stickerId": "1"
  }
}
] }
|]

goodFollow :: BL.ByteString
goodFollow = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "follow",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  }
}
] }
|]

badFollow :: BL.ByteString
badFollow = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "follow'",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  }
}
] }
|]

goodUnfollow :: BL.ByteString
goodUnfollow = [r|
{ "events": [
{
  "type": "unfollow",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  }
}
] }
|]

badUnfollow :: BL.ByteString
badUnfollow = [r|
{ "events": [
{
  "type": "unfollow'",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  }
}
] }
|]

goodJoin :: BL.ByteString
goodJoin = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "join",
  "timestamp": 1462629479859,
  "source": {
    "type": "group",
    "groupId": "cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  }
}
] }
|]

badJoin :: BL.ByteString
badJoin = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "join'",
  "timestamp": 1462629479859,
  "source": {
    "type": "group",
    "groupId": "cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  }
}
] }
|]

goodLeave :: BL.ByteString
goodLeave = [r|
{ "events": [
{
  "type": "leave",
  "timestamp": 1462629479859,
  "source": {
    "type": "group",
    "groupId": "cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  }
}
] }
|]

badLeave :: BL.ByteString
badLeave = [r|
{ "events": [
{
  "type": "leave'",
  "timestamp": 1462629479859,
  "source": {
    "type": "group",
    "groupId": "cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  }
}
] }
|]

goodPostback :: BL.ByteString
goodPostback = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "postback",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "postback": {
    "data": "action=buyItem&itemId=123123&color=red"
  }
}
] }
|]

badPostback :: BL.ByteString
badPostback = [r|
{ "events": [
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "postback",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U206d25c2ea6bd87c17655609a1c37cb8"
  },
  "postback": {
    "data'": "action=buyItem&itemId=123123&color=red"
  }
}
] }
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

goodBeaconLeave :: BL.ByteString
goodBeaconLeave = [r|
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
    "type": "leave"
  }
}
] }
|]

goodBeaconBanner :: BL.ByteString
goodBeaconBanner = [r|
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
    "type": "banner"
  }
}
] }
|]

goodBeaconWithDm :: BL.ByteString
goodBeaconWithDm = [r|
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
    "type": "enter",
    "dm": "i am a direct message."
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
