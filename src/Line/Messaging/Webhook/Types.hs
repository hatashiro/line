module Line.Messaging.Webhook.Types (
  module Line.Messaging.Common.Types,
  WebhookResult (..),
  WebhookFailure (..),
  ReplyToken,
  Body (..),
  Event (..),
  EventTuple,
  ReplyableEvent,
  NonReplyableEvent,
  getSource,
  getDatetime,
  getReplyToken,
  getMessage,
  getPostback,
  getBeacon,
  EventSource (..),
  getId,
  EventMessage (..),
  BeaconData (..),
  ) where

import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Line.Messaging.API.Types
import Line.Messaging.Common.Types
import Network.Wai (Response, Application)
import qualified Data.Text as T

data WebhookResult = Ok
                   | WaiResponse Response
                   | WaiApp Application

data WebhookFailure = SignatureVerificationFailed
                    | MessageDecodeFailed
                    deriving (Eq, Show)

type ReplyToken = T.Text

-- The Event data type and instances for proper type classes (e.g. FromJson)
-- should be implemented here.
-- For the Event spec, please refer to the official doc.
--
-- https://devdocs.line.me/en/#webhook-event-object

newtype Body = Body [Event]
             deriving (Eq, Show)

instance FromJSON Body where
  parseJSON (Object v) = Body <$> v .: "events"
  parseJSON _ = fail "Body"

data Event = MessageEvent (ReplyableEvent EventMessage)
           | FollowEvent (ReplyableEvent ())
           | UnfollowEvent (NonReplyableEvent ())
           | JoinEvent (ReplyableEvent ())
           | LeaveEvent (NonReplyableEvent ())
           | PostbackEvent (ReplyableEvent T.Text)
           | BeaconEvent (ReplyableEvent BeaconData)
           deriving (Eq, Show)

type EventTuple r a = (EventSource, UTCTime, r, a)
type ReplyableEvent a = EventTuple ReplyToken a
type NonReplyableEvent a = EventTuple () a

getSource :: EventTuple r a -> EventSource
getSource (s, _, _, _) = s

getDatetime :: EventTuple r a -> UTCTime
getDatetime (_, t, _, _) = t

getReplyToken :: ReplyableEvent a -> ReplyToken
getReplyToken (_, _, r, _) = r

getMessage :: ReplyableEvent EventMessage -> EventMessage
getMessage (_, _, _, m) = m

getPostback :: ReplyableEvent T.Text -> T.Text
getPostback (_, _, _, d) = d

getBeacon :: ReplyableEvent BeaconData -> BeaconData
getBeacon (_, _, _, b) = b

instance FromJSON Event where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "message" -> MessageEvent <$> (replyable v <*> v .: "message")
      "follow" -> FollowEvent <$> (replyable v <*> none)
      "unfollow" -> UnfollowEvent <$> (nonReplyable v <*> none)
      "join" -> JoinEvent <$> (replyable v <*> none)
      "leave" -> LeaveEvent <$> (nonReplyable v <*> none)
      "postback" -> PostbackEvent <$> (replyable v <*> ((v .: "postback") >>= (.: "data")))
      "beacon" -> BeaconEvent <$> (replyable v <*> v .: "beacon")
      _ -> fail "Event"
    where
      common o = (,,,) <$> (o .: "source")
                       <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> o .: "timestamp")
      withReplyToken p o = p <*> o .: "replyToken"
      none = return ()
      replyable o = common o `withReplyToken` o
      nonReplyable o = common o <*> none

  parseJSON _ = fail "Event"

data EventSource = User ID
                 | Group ID
                 | Room ID
                 deriving (Eq, Show)

getId :: EventSource -> ID
getId (User i) = i
getId (Group i) = i
getId (Room i) = i

instance FromJSON EventSource where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "user" -> User <$> v .: "userId"
      "group" -> Group <$> v .: "groupId"
      "room" -> Room <$> v .: "roomId"
      _ -> fail "EventSource"
  parseJSON _ = fail "EventSource"

data EventMessage = TextEM ID Text
                  | ImageEM ID
                  | VideoEM ID
                  | AudioEM ID
                  | LocationEM ID Location
                  | StickerEM ID Sticker
                  deriving (Eq, Show)

instance FromJSON EventMessage where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "text" -> TextEM <$> v .: "id" <*> parseJSON (Object v)
      "image" -> ImageEM <$> v .: "id"
      "video" -> VideoEM <$> v .: "id"
      "audio" -> AudioEM <$> v .: "id"
      "location" -> LocationEM <$> v .: "id" <*> parseJSON (Object v)
      "sticker" -> StickerEM <$> v .: "id" <*> parseJSON (Object v)
      _ -> fail "EventMessage"
  parseJSON _ = fail "IncommingMessage"

data BeaconData = BeaconEnter { getHWID :: ID }
                deriving (Eq, Show)

instance FromJSON BeaconData where
  parseJSON (Object v) = v .: "type" >>= \ t ->
    case t :: T.Text of
      "enter" -> BeaconEnter <$> v .: "hwid"
      _ -> fail "BeaconData"
  parseJSON _ = fail "BeaconData"
