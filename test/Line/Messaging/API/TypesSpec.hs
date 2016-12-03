module Line.Messaging.API.TypesSpec where

import Test.Hspec

import Line.Messaging.API.TypesSpecHelper

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (isJust)
import Line.Messaging.API.Types
import Text.RawString.QQ
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
                -> [Pair]
                -> SpecWith ()
messageableSpec testTitle a result =
  it testTitle $ toJSON (Message a) `shouldBe` object result

spec :: Spec
spec = do
  describe "messageables" $ do
    messageableSpec "text"
      (Text "hello")
      [ "type" .= ("text" :: T.Text)
      , "text" .= ("hello" :: T.Text)
      ]

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
