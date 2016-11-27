{-|
This module provides types to be used with "Line.Messaging.API".
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Line.Messaging.API.Types (
  -- * Common types
  -- | Re-exported for convenience.
  module Line.Messaging.Common.Types,
  -- * Message types
  Messageable,
  Message (..),
  -- ** Text
  Text (..),
  -- ** Image
  Image (..),
  -- ** Video
  Video (..),
  -- ** Audio
  Audio (..),
  -- ** Location
  Location (..),
  -- ** Sticker
  Sticker (..),
  -- ** Image map
  ImageMap (..),
  ImageMapAction (..),
  ImageMapArea,
  -- ** Template
  Template (..),
  Buttons (..),
  Confirm (..),
  Carousel (..),
  Column (..),
  Label,
  TemplateAction (..),
  -- * Profile
  Profile (..),
  -- * Error types
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

-- | A type class representing types to be converted into 'Message'.
--
-- It has @toType@ and @toObject@ as its minimal complete definition, but it
-- is not recommended to define any extra instance, as the new type may not work
-- with the LINE APIs.
--
-- About existing messageable types, please refer to the following instances.
-- Each instance is matched with a message object described in
-- <https://devdocs.line.me/en/#send-message-object the LINE documentation>.
class Messageable a where
  toType :: a -> T.Text
  toObject :: a -> [Pair]

  toValue :: a -> Value
  toValue a = object $ ("type" .= toType a) : toObject a

-- | A type representing a message to be sent.
--
-- The data constructor converts 'Messageable' into 'Message'. It allows
-- different types of 'Messageable' to be sent through a single API call.
--
-- An example usage is like below.
--
-- @
-- pushTextAndImage :: ID -> APIIO ()
-- pushTextAndImage identifier = push identifier [
--   Message $ 'Text' "hello",
--   Message $ 'Image' "https://example.com/image.jpg" "https://example.com/preview.jpg"
--   ]
-- @
data Message = forall a. (Show a, Messageable a) => Message a

deriving instance Show Message

instance ToJSON Message where
  toJSON (Message m) = toValue m

-- | 'Messageable' for text data.
--
-- It contains 'T.Text' of "Data.Text". Its corresponding JSON spec is described
-- <https://devdocs.line.me/en/#text here>.
--
-- This type is also used to decode text event message from webhook request.
-- About the webhook usage, please refer to
-- <./Line-Messaging-Webhook-Types.html#t:EventMessage EventMessage> in
-- "Line.Messaging.Webhook.Types".
newtype Text = Text { getText :: T.Text }
             deriving (Eq, Ord, Show)

instance FromJSON Text where
  parseJSON (Object v) = Text <$> v .: "text"
  parseJSON (String text) = return $ Text text
  parseJSON _ = fail "Text"

instance Messageable Text where
  toType _ = "text"
  toObject (Text text) = [ "text" .= text ]

-- | 'Messageable' for image data.
--
-- It contains URLs of an original image and its preview. Its corresponding JSON
-- spec is described <https://devdocs.line.me/en/#image here>.
data Image = Image { getImageURL :: URL
                   , getImagePreviewURL :: URL
                   }
             deriving (Eq, Show)

instance Messageable Image where
  toType _ = "image"
  toObject (Image original preview) = [ "originalContentUrl" .= original
                                      , "previewImageUrl" .= preview
                                      ]

-- | 'Messageable' for video data.
--
-- It contains URLs of an original video and its preview. Its corresponding JSON
-- spec is described <https://devdocs.line.me/en/#video here>.
data Video = Video { getVideoURL :: URL
                   , getVideoPreviewURL :: URL
                   }
             deriving (Eq, Show)

instance Messageable Video where
  toType _ = "video"
  toObject (Video original preview) = [ "originalContentUrl" .= original
                                      , "previewImageUrl" .= preview
                                      ]

-- | 'Messageable' for audio data.
--
-- It contains a URL of an audio, and its duration in milliseconds. Its
-- corresponding JSON spec is described
-- <https://devdocs.line.me/en/#audio here>.
data Audio = Audio { getAudioURL :: URL
                   , getAudioDuration :: Integer
                   }
             deriving (Eq, Show)

instance Messageable Audio where
  toType _ = "audio"
  toObject (Audio original duration) = [ "originalContentUrl" .= original
                                       , "duration" .= duration
                                       ]

-- | 'Messageable' for location data.
--
-- It contains a title, address, and geographic coordination of a location.
-- Its corresponding JSON spec is described
-- <https://devdocs.line.me/en/#location here>.
--
-- This type is also used to decode location event message from webhook request.
-- About the webhook usage, please refer to
-- <./Line-Messaging-Webhook-Types.html#t:EventMessage EventMessage> in
-- "Line.Messaging.Webhook.Types".
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

-- | 'Messageable' for sticker data.
--
-- It contains its package and sticker ID.  Its corresponding JSON spec is
-- described <https://devdocs.line.me/en/#sticker here>.
--
-- This type is also used to decode sticker event message from webhook request.
-- About the webhook usage, please refer to
-- <./Line-Messaging-Webhook-Types.html#t:EventMessage EventMessage> in
-- "Line.Messaging.Webhook.Types".
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

-- | 'Messageable' for image map data.
--
-- About how to send an image map message and what each field means, please
-- refer to
-- <https://devdocs.line.me/en/#imagemap-message image map message spec>.
data ImageMap = ImageMap { getBaseImageURL :: URL
                           -- ^ A <https://devdocs.line.me/en/#base-url base URL> of images.
                         , getIMAltText :: T.Text
                           -- ^ An alt text for devices not supporting image map.
                         , getBaseImageSize :: (Integer, Integer)
                           -- ^ An image size tuple, (width, height) specifically.
                           -- The width to be set to 1040, the height to be set to
                           -- the value corresponding to a width of 1040.
                         , getIMActions :: [ImageMapAction]
                           -- ^ Actions to be executed when each area is tapped.
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

-- | A type representing actions when a specific area of an image map is tapped.
--
-- It contains action data and area information.
data ImageMapAction = IMURIAction URL ImageMapArea
                      -- ^ Open a web page when an area is tapped.
                    | IMMessageAction T.Text ImageMapArea
                      -- ^ Send a text message from the user who tapped an area.
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

-- | A type representing a tappable area in an image map
--
-- Each component means (x, y, width, height) correspondingly.
type ImageMapArea = (Integer, Integer, Integer, Integer)

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

data TemplateAction = TplPostbackAction Label Postback T.Text
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

-- | A type to represent a user's profile.
--
-- It is the return type of the <./Line-Messaging-API.html#v:getProfile getProfile>
-- API in the "Line.Messaging.API" module.
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

-- | An error type possibly returned from the
-- @<./Line-Messaging-API.html#t:APIIO APIIO>@ type.
--
-- State code errors may contain a parsed error body. Other types of errors,
-- which may rarely occur if used properly, does not.
--
-- For more details of error types, please refer to
-- <https://devdocs.line.me/en/#status-codes Status codes> and
-- <https://devdocs.line.me/en/#error-response Error response> sections in the
-- LINE documentation.
data APIError = BadRequest (Maybe APIErrorBody)
              -- ^ 400 Bad Request with a parsed error body, caused by badly
              -- formatted request.
              | Unauthorized (Maybe APIErrorBody)
              -- ^ 401 Unauthorized with a parsed error body, caused by invalid
              -- access token.
              | Forbidden (Maybe APIErrorBody)
              -- ^ 403 Forbidden with a parsed error body, caused by
              -- unauthorized account or plan.
              | TooManyRequests (Maybe APIErrorBody)
              -- ^ 429 Too Many Requests with a parsed error body, caused by
              -- exceeding the <https://devdocs.line.me/en/#rate-limits rate limit>.
              | InternalServerError (Maybe APIErrorBody)
              -- ^ 500 Internal Server Error with a parsed error body.
              | UndefinedStatusCode Int BL.ByteString
              -- ^ Caused by status codes other than 200 and listed statuses
              -- above. With the status code and request body.
              | JSONDecodeError String
              -- ^ Caused by badly formatted response body from APIs returning
              -- meaningful data.
              | UndefinedError SomeException
              -- ^ Any other exception caught as 'SomeException'.
              deriving Show

-- | An error body type.
--
-- It contains an error message, and possibly a property information and more
-- detailed error bodies.
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
