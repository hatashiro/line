{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Line.Messaging.API.Types (
  module Line.Messaging.Common.Types,
  Messageable,
  Message (..),
  Text (..),
  Image (..),
  Video (..),
  Audio (..),
  Location (..),
  Sticker (..),
  ImageMap (..),
  ImageMapAction (..),
  ImageMapArea,
  Template (..),
  Buttons (..),
  Confirm (..),
  Carousel (..),
  Column (..),
  Label,
  TemplateAction (..),
  Profile (..),
  APIError (..),
  APIErrorBody (..),
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.:), (.=))
import Data.Aeson.Types (Pair)
import Line.Messaging.Common.Types
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

class Messageable a where
  toType :: a -> T.Text
  toObject :: a -> [Pair]

  toValue :: a -> Value
  toValue a = object $ ("type" .= toType a) : toObject a

data Message = forall a. (Show a, Messageable a) => Message a

deriving instance Show Message

instance ToJSON Message where
  toJSON (Message m) = toValue m

newtype Text = Text { getText :: T.Text }
             deriving (Eq, Ord, Show)

instance FromJSON Text where
  parseJSON (Object v) = Text <$> v .: "text"
  parseJSON (String text) = return $ Text text
  parseJSON _ = fail "Text"

instance Messageable Text where
  toType _ = "text"
  toObject (Text text) = [ "text" .= text ]

data Image = Image { getImageURL :: URL
                   , getImagePreviewURL :: URL
                   }
             deriving (Eq, Show)

instance Messageable Image where
  toType _ = "image"
  toObject (Image original preview) = [ "originalContentUrl" .= original
                                      , "previewImageUrl" .= preview
                                      ]

data Video = Video { getVideoURL :: URL
                   , getVideoPreviewURL :: URL
                   }
             deriving (Eq, Show)

instance Messageable Video where
  toType _ = "video"
  toObject (Video original preview) = [ "originalContentUrl" .= original
                                      , "previewImageUrl" .= preview
                                      ]

data Audio = Audio { getAudioURL :: URL
                   , getAudioDuration :: Integer
                   }
             deriving (Eq, Show)

instance Messageable Audio where
  toType _ = "audio"
  toObject (Audio original duration) = [ "originalContentUrl" .= original
                                       , "duration" .= duration
                                       ]

data Location = Location { getLocationTitle :: T.Text
                         , getAddress :: T.Text
                         , getLatitude :: Double
                         , getLongitude :: Double
                         }
                deriving (Eq, Show)

instance FromJSON Location where
  parseJSON (Object v) = Location <$> v .: "title"
                                  <*> v .: "address"
                                  <*> v .: "latitude"
                                  <*> v .: "longitude"
  parseJSON _ = fail "Location"

instance Messageable Location where
  toType _ = "location"
  toObject (Location title address latitude longitude) = [ "title" .= title
                                                         , "address" .= address
                                                         , "latitude" .= latitude
                                                         , "longitude" .= longitude
                                                         ]

data Sticker = Sticker { getPackageID :: ID
                       , getStickerID :: ID
                       }
               deriving (Eq, Show)

instance FromJSON Sticker where
  parseJSON (Object v) = Sticker <$> v .: "packageId"
                                 <*> v .: "stickerId"
  parseJSON _ = fail "Sticker"

instance Messageable Sticker where
  toType _ = "sticker"
  toObject (Sticker packageId stickerId) = [ "packageId" .= packageId
                                           , "stickerId" .= stickerId
                                           ]

data ImageMap = ImageMap { getBaseImageURL :: URL
                         , getIMAltText :: T.Text
                         , getBaseImageSize :: (Integer, Integer) -- set w h to 1040
                         , getIMActions :: [ImageMapAction]
                         }
                deriving (Eq, Show)

instance Messageable ImageMap where
  toType _ = "imagemap"
  toObject (ImageMap url alt (w, h) as) = [ "baseUrl" .= url
                                          , "altText" .= alt
                                          , "baseSize" .= object [ "width" .= w
                                                                 , "height" .= h
                                                                 ]
                                          , "actions" .= toJSON as
                                          ]

data ImageMapAction = IMURIAction URL ImageMapArea
                    | IMMessageAction T.Text ImageMapArea
                    deriving (Eq, Show)

instance ToJSON ImageMapAction where
  toJSON (IMURIAction uri area) = object [ "type" .= ("uri" :: T.Text)
                                               , "linkUri" .= uri
                                               , "area" .= toAreaJSON area
                                               ]
  toJSON (IMMessageAction text area) = object [ "type" .= ("message" :: T.Text)
                                                    , "text" .= text
                                                    , "area" .= toAreaJSON area
                                                    ]

type ImageMapArea = (Integer, Integer, Integer, Integer) -- x y width height

toAreaJSON :: ImageMapArea -> Value
toAreaJSON (x, y, w, h) = object [ "x" .= x, "y" .= y, "width" .= w, "height" .= h ]

data Template t = Template { getTemplateAltText :: T.Text
                           , getTemplate :: t
                           }
                  deriving (Eq, Show)

templateType :: Template a -> T.Text
templateType _ = "template"

templateToObject :: ToJSON a => Template a -> [Pair]
templateToObject (Template alt a) = [ "altText" .= alt
                                    , "template" .= toJSON a
                                    ]

instance Messageable (Template Buttons) where
  toType = templateType
  toObject = templateToObject

instance Messageable (Template Confirm) where
  toType = templateType
  toObject = templateToObject

instance Messageable (Template Carousel) where
  toType = templateType
  toObject = templateToObject

data Buttons = Buttons { getButtonsThumbnailURL :: URL
                       , getButtonsTitle :: T.Text
                       , getButtonsText :: T.Text
                       , getButtonsActions :: [TemplateAction]
                       }
               deriving (Eq, Show)

instance ToJSON Buttons where
  toJSON (Buttons url title text actions) = object [ "type" .= ("buttons" :: T.Text)
                                                   , "thumbnailImageUrl" .= url
                                                   , "title" .= title
                                                   , "text" .= text
                                                   , "actions" .= toJSON actions
                                                   ]

data Confirm = Confirm { getConfirmText :: T.Text
                       , getConfirmActions :: [TemplateAction]
                       }
               deriving (Eq, Show)

instance ToJSON Confirm where
  toJSON (Confirm text actions) = object [ "type" .= ("confirm" :: T.Text)
                                         , "text" .= text
                                         , "actions" .= toJSON actions
                                         ]

data Carousel = Carousel { getColumns :: [Column] }
              deriving (Eq, Show)

instance ToJSON Carousel where
  toJSON (Carousel columns) = object [ "type" .= ("carousel" :: T.Text)
                                     , "columns" .= toJSON columns
                                     ]

data Column = Column { getColumnThumbnailURL :: URL
                     , getColumnTitle :: T.Text
                     , getColumnText :: T.Text
                     , getColumnActions :: [TemplateAction]
                     }
              deriving (Eq, Show)

instance ToJSON Column where
  toJSON (Column url title text actions) = object [ "thumbnailImageUrl" .= url
                                                  , "title" .= title
                                                  , "text" .= text
                                                  , "actions" .= toJSON actions
                                                  ]

type Label = T.Text

data TemplateAction = TplPostbackAction Label T.Text T.Text
                    | TplMessageAction Label T.Text
                    | TplURIAction Label URL
                    deriving (Eq, Show)

instance ToJSON TemplateAction where
  toJSON (TplPostbackAction label data' text) = object [ "type" .= ("postback" :: T.Text)
                                                       , "label" .= label
                                                       , "data" .= data'
                                                       , "text" .= text
                                                       ]
  toJSON (TplMessageAction label text) = object [ "type" .= ("message" :: T.Text)
                                                , "label" .= label
                                                , "text" .= text
                                                ]
  toJSON (TplURIAction label uri) = object [ "type" .= ("uri" :: T.Text)
                                           , "label" .= label
                                           , "uri" .= uri
                                           ]

data Profile = Profile { getUserID :: ID
                       , getDisplayName :: T.Text
                       , getPictureURL :: Maybe URL
                       , getStatusMessage :: Maybe T.Text
                       }
               deriving (Eq, Show)

instance FromJSON Profile where
  parseJSON (Object v) = Profile <$> v .: "userId"
                                 <*> v .: "displayName"
                                 <*> v .: "pictureUrl"
                                 <*> v .: "statusMessage"
  parseJSON _ = fail "Profile"

data APIError = BadRequest (Maybe APIErrorBody)
              | Unauthorized (Maybe APIErrorBody)
              | Forbidden (Maybe APIErrorBody)
              | TooManyRequests (Maybe APIErrorBody)
              | InternalServerError (Maybe APIErrorBody)
              | UndefinedStatusCode Int BL.ByteString
              | JSONDecodeError String
              | UndefinedError SomeException
              deriving Show

data APIErrorBody = APIErrorBody { getErrorMessage :: T.Text
                                 , getErrorProperty :: Maybe T.Text
                                 , getErrorDetails :: Maybe [APIErrorBody]
                                 }
                  deriving Show

instance FromJSON APIErrorBody where
  parseJSON (Object v) = APIErrorBody <$> v .: "message"
                                      <*> (v .: "property" <|> return Nothing)
                                      <*> (v .: "details" <|> return Nothing)
  parseJSON _ = fail "APIErrorBody"
