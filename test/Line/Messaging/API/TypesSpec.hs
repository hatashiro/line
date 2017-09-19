module Line.Messaging.API.TypesSpec where

import Test.Hspec

import Line.Messaging.API.TypesSpecHelper

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (isJust, fromJust)
import Line.Messaging.API.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

fromJSONSpec :: (FromJSON a, Eq a, Show a)
             => [(BL.ByteString, Maybe a)]
             -> SpecWith ()
fromJSONSpec ((raw, result):xs) = do
  let title = if isJust result then "success" else "fail"
  it title $ decode raw `shouldBe` result
  fromJSONSpec xs
fromJSONSpec [] = return ()

messageableSpec :: (Messageable a, Eq a, Show a)
                => String
                -> a
                -> BL.ByteString
                -> SpecWith ()
messageableSpec testTitle a result =
  it testTitle $ toJSON (Message a) `shouldBe` fromJust (decode result)

spec :: Spec
spec = do
  describe "messageables" $ do
    messageableSpec "text"
      (Text "Hello, world")
      textMessageableResult

    messageableSpec "image"
      (Image "https://example.com/original.jpg" "https://example.com/preview.jpg")
      imageMessageableResult

    messageableSpec "video"
      (Video "https://example.com/original.mp4" "https://example.com/preview.jpg")
      videoMessageableResult

    messageableSpec "audio"
      (Audio "https://example.com/original.m4a" 240000)
      audioMessageableResult

    messageableSpec "location"
      (Location "my location" "some address" 35.65910807942215 139.70372892916203)
      locationMessageableResult

    messageableSpec "sticker"
      (Sticker "1" "1")
      stickerMessageableResult

    messageableSpec "imagemap"
      (ImageMap "https://example.com/bot/images/rm001" "this is an imagemap" (1040, 1040)
       [ IMURIAction "https://example.com/" (0, 0, 520, 1040)
       , IMMessageAction "hello" (520, 0, 520, 1040)
       ])
      imageMapMessageableResult

    messageableSpec "template buttons"
      (Template "this is a buttons template" $
       Buttons (Just "https://example.com/bot/images/image.jpg") (Just "Menu") "Please select"
       [ TplPostbackAction "Buy" "action=buy&itemid=123" Nothing
       , TplPostbackAction "Add to cart" "action=add&itemid=123" Nothing
       , TplURIAction "View detail" "http://example.com/page/123"
       , TplDatetimePickerAction (Just "yes label") "pickpick" Datetime Nothing Nothing Nothing
       ])
      buttonsTemplateMessageableResult

    messageableSpec "template confirm"
      (Template "this is a confirm template" $
       Confirm "Are you sure?"
       [ TplMessageAction "Yes" "yes"
       , TplMessageAction "No" "no"
       , TplDatetimePickerAction Nothing "pickpick2" Date (Just "1990-01-01") (Just "2100-12-31") (Just "1900-01-01")
       ])
      confirmTemplateMessageableResult

    messageableSpec "template carousel"
      (Template "this is a carousel template" $
       Carousel [ Column (Just "https://example.com/bot/images/item1.jpg")
                         (Just "this is menu")
                         "description"
                         [ TplPostbackAction "Buy" "action=buy&itemid=111" Nothing
                         , TplPostbackAction "Add to cart" "action=add&itemid=111" Nothing
                         , TplURIAction "View detail" "http://example.com/page/111"
                         , TplDatetimePickerAction Nothing "pickpick3" Time Nothing (Just "23:23") (Just "11:11")
                         ]
                , Column (Just "https://example.com/bot/images/item2.jpg")
                         (Just "this is menu")
                         "description"
                         [ TplPostbackAction "Buy" "action=buy&itemid=222" Nothing
                         , TplPostbackAction "Add to cart" "action=add&itemid=222" Nothing
                         , TplURIAction "View detail" "http://example.com/page/222"
                         ]
                ])
      carouselTemplateMessageableResult

    messageableSpec "template image carousel"
      (Template "this is an image carousel template" $
       ImageCarousel [ ImageColumn "https://example.com/bot/images/item1.jpg"
                         (TplPostbackAction "Buy" "action=buy&itemid=111" Nothing)
                     , ImageColumn "https://example.com/bot/images/item2.jpg"
                         (TplPostbackAction "Buy" "action=buy&itemid=222" Nothing)
                     ]
      )
      imageCarouselTemplateMessageableResult

  describe "JSON decode" $ do
    describe "profile" $ fromJSONSpec
      [ ( fullProfile, Just $ Profile
                                "123"
                                "Jun"
                                (Just "https://example.com/profile.jpg")
                                (Just "some status message") )
      , ( noPicProfile, Just $ Profile
                                 "123"
                                 "Jun"
                                 Nothing
                                 (Just "some status message") )
      , ( noDescProfile, Just $ Profile
                                  "123"
                                  "Jun"
                                  (Just "https://example.com/profile.jpg")
                                  Nothing )
      , ( simpleProfile, Just $ Profile
                                  "123"
                                  "Jun"
                                  Nothing
                                  Nothing )
      , ( badProfile, Nothing )
      ]

    describe "API error body" $ fromJSONSpec
      [ ( simpleError, Just $ APIErrorBody "Invalid reply token" Nothing Nothing )
      , ( complexError, Just $ APIErrorBody
                                 "The request body has 2 error(s)"
                                 Nothing
                                 (Just [ APIErrorBody "May not be empty" (Just "messages[0].text") Nothing
                                       , APIErrorBody "Must be one of the following values: [text, image, video, audio, location, sticker, template, imagemap]" (Just "messages[1].type") Nothing
                                       ])
        )
      , ( badError, Nothing )
      ]
