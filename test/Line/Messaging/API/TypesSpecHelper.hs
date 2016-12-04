{-# LANGUAGE QuasiQuotes #-}

module Line.Messaging.API.TypesSpecHelper where

import Text.RawString.QQ
import qualified Data.ByteString.Lazy as BL

textMessageableResult :: BL.ByteString
textMessageableResult = [r|
{
    "type": "text",
    "text": "Hello, world"
}
|]

imageMessageableResult :: BL.ByteString
imageMessageableResult = [r|
{
    "type": "image",
    "originalContentUrl": "https://example.com/original.jpg",
    "previewImageUrl": "https://example.com/preview.jpg"
}
|]

videoMessageableResult :: BL.ByteString
videoMessageableResult = [r|
{
    "type": "video",
    "originalContentUrl": "https://example.com/original.mp4",
    "previewImageUrl": "https://example.com/preview.jpg"
}
|]

audioMessageableResult :: BL.ByteString
audioMessageableResult = [r|
{
    "type": "audio",
    "originalContentUrl": "https://example.com/original.m4a",
    "duration": 240000
}
|]

locationMessageableResult :: BL.ByteString
locationMessageableResult = [r|
{
    "type": "location",
    "title": "my location",
    "address": "some address",
    "latitude": 35.65910807942215,
    "longitude": 139.70372892916203
}
|]

stickerMessageableResult :: BL.ByteString
stickerMessageableResult = [r|
{
  "type": "sticker",
  "packageId": "1",
  "stickerId": "1"
}
|]

imageMapMessageableResult :: BL.ByteString
imageMapMessageableResult = [r|
{
  "type": "imagemap",
  "baseUrl": "https://example.com/bot/images/rm001",
  "altText": "this is an imagemap",
  "baseSize": {
      "height": 1040,
      "width": 1040
  },
  "actions": [
      {
          "type": "uri",
          "linkUri": "https://example.com/",
          "area": {
              "x": 0,
              "y": 0,
              "width": 520,
              "height": 1040
          }
      },
      {
          "type": "message",
          "text": "hello",
          "area": {
              "x": 520,
              "y": 0,
              "width": 520,
              "height": 1040
          }
      }
  ]
}
|]

buttonsTemplateMessageableResult :: BL.ByteString
buttonsTemplateMessageableResult = [r|
{
  "type": "template",
  "altText": "this is a buttons template",
  "template": {
      "type": "buttons",
      "thumbnailImageUrl": "https://example.com/bot/images/image.jpg",
      "title": "Menu",
      "text": "Please select",
      "actions": [
          {
            "type": "postback",
            "label": "Buy",
            "data": "action=buy&itemid=123"
          },
          {
            "type": "postback",
            "label": "Add to cart",
            "data": "action=add&itemid=123"
          },
          {
            "type": "uri",
            "label": "View detail",
            "uri": "http://example.com/page/123"
          }
      ]
  }
}
|]

confirmTemplateMessageableResult :: BL.ByteString
confirmTemplateMessageableResult = [r|
{
  "type": "template",
  "altText": "this is a confirm template",
  "template": {
      "type": "confirm",
      "text": "Are you sure?",
      "actions": [
          {
            "type": "message",
            "label": "Yes",
            "text": "yes"
          },
          {
            "type": "message",
            "label": "No",
            "text": "no"
          }
      ]
  }
}
|]

carouselTemplateMessageableResult :: BL.ByteString
carouselTemplateMessageableResult = [r|
{
  "type": "template",
  "altText": "this is a carousel template",
  "template": {
      "type": "carousel",
      "columns": [
          {
            "thumbnailImageUrl": "https://example.com/bot/images/item1.jpg",
            "title": "this is menu",
            "text": "description",
            "actions": [
                {
                    "type": "postback",
                    "label": "Buy",
                    "data": "action=buy&itemid=111"
                },
                {
                    "type": "postback",
                    "label": "Add to cart",
                    "data": "action=add&itemid=111"
                },
                {
                    "type": "uri",
                    "label": "View detail",
                    "uri": "http://example.com/page/111"
                }
            ]
          },
          {
            "thumbnailImageUrl": "https://example.com/bot/images/item2.jpg",
            "title": "this is menu",
            "text": "description",
            "actions": [
                {
                    "type": "postback",
                    "label": "Buy",
                    "data": "action=buy&itemid=222"
                },
                {
                    "type": "postback",
                    "label": "Add to cart",
                    "data": "action=add&itemid=222"
                },
                {
                    "type": "uri",
                    "label": "View detail",
                    "uri": "http://example.com/page/222"
                }
            ]
          }
      ]
  }
}
|]

fullProfile :: BL.ByteString
fullProfile = [r|
{
  "userId": "123",
  "displayName": "Jun",
  "pictureUrl": "https://example.com/profile.jpg",
  "statusMessage": "some status message"
}
|]

noPicProfile :: BL.ByteString
noPicProfile = [r|
{
  "userId": "123",
  "displayName": "Jun",
  "pictureUrl": null,
  "statusMessage": "some status message"
}
|]

noDescProfile :: BL.ByteString
noDescProfile = [r|
{
  "userId": "123",
  "displayName": "Jun",
  "pictureUrl": "https://example.com/profile.jpg",
  "statusMessage": null
}
|]

simpleProfile :: BL.ByteString
simpleProfile = [r|
{
  "userId": "123",
  "displayName": "Jun",
  "pictureUrl": null,
  "statusMessage": null
}
|]

badProfile :: BL.ByteString
badProfile = [r|
{
  "userId'": "123",
  "displayName": "Jun",
  "pictureUrl": "https://example.com/profile.jpg",
  "statusMessage": null
}
|]

simpleError :: BL.ByteString
simpleError = [r|
{
  "message":"Invalid reply token"
}
|]


complexError :: BL.ByteString
complexError = [r|
{
  "message":"The request body has 2 error(s)",
  "details":[
    {"message":"May not be empty","property":"messages[0].text"},
    {"message":"Must be one of the following values: [text, image, video, audio, location, sticker, template, imagemap]","property":"messages[1].type"}
  ]
}
|]

badError :: BL.ByteString
badError = [r|
{
  "massage": "no message, yes massage"
}
|]
