{-# LANGUAGE QuasiQuotes #-}

module Line.Messaging.API.TypesSpecHelper where

import Text.RawString.QQ
import qualified Data.ByteString.Lazy as BL

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
