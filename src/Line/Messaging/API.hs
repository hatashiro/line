{-|
This module provides functions corresponding to the LINE Messaging APIs, nearly
one on one.

For more details about the APIs themselves, please refer to the
<https://developers.line.me/en/docs/messaging-api/reference/ API references>.
-}

module Line.Messaging.API (
  -- * Types
  -- | Re-exported for convenience.
  module Line.Messaging.API.Types,
  -- * Monad transformer for APIs
  APIIO,
  runAPI,
  -- * LINE Messaging APIs
  -- | Every API call returns its result with @'APIIO'@. About the usage of
  -- @'APIIO'@, please refer to the previous section.
  push,
  multicast,
  reply,
  getContent,
  getProfile,
  getGroupMemberProfile,
  getRoomMemberProfile,
  getGroupMemberIDs,
  getRoomMemberIDs,
  leaveRoom,
  leaveGroup,
  ) where

import Control.Exception (SomeException(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Control.Monad.Trans.Except (runExceptT, ExceptT, throwE, catchE)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.API.Types
import Line.Messaging.Types (ReplyToken)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- | A monad transformer for API calls. If translated into a human-readable
-- form, it means:
--
-- 1. An API call needs a channel access token to specify through which
--    channel it should send the call (@'ReaderT' 'ChannelAccessToken'@).
-- 2. An API call effectfully returns a result if successful, @'APIError'@
--    otherwise (@'ExceptT' 'APIError'@).
type APIIO a = ReaderT ChannelAccessToken (ExceptT APIError IO) a

-- | 'runAPI' resolves the 'APIIO' monad transformer, and turns it into a plain
-- @'IO'@ with @'ChannelAccessToken'@ provided.
--
-- The reason the type of the first parameter is not @'ChannelAccessToken'@, but
-- @'IO' 'ChannelAccessToken'@, is that it is usually loaded via effectful
-- actions such as parsing command line arguments or reading a config file.
--
-- An example usage is like below:
--
-- @
-- api :: APIIO a -> IO (Either APIError a)
-- api = runAPI getChannelAccessTokenFromConfig
--
-- main :: IO ()
-- main = do
--   result <- api $ push "some_receiver_id" [ Message $ Text "Hello, world!" ]
--   case result of
--     Right _ -> return ()
--     Left err -> print err
-- @
runAPI :: IO ChannelAccessToken -> APIIO a -> IO (Either APIError a)
runAPI getToken api = getToken >>= runExceptT . runReaderT api

createReq :: B.ByteString -> String -> APIIO Request
createReq method url = do
  token <- encodeUtf8 <$> ask
  setRequestIgnoreStatus .
    setRequestMethod method .
    addRequestHeader "Authorization" ("Bearer " `B.append` token)
    <$> parseRequest url

handleError :: SomeException -> (ExceptT APIError IO) a
handleError = throwE . UndefinedError

runReq :: Request -> APIIO BL.ByteString
runReq req = lift $ do
  res <- httpLBS req `catchE` handleError
  let status = statusCode $ getResponseStatus res
  let body = getResponseBody res
  case status of
    200 -> return $ body
    400 -> throwE $ BadRequest (decode' body)
    401 -> throwE $ Unauthorized (decode' body)
    403 -> throwE $ Forbidden (decode' body)
    429 -> throwE $ TooManyRequests (decode' body)
    500 -> throwE $ InternalServerError (decode' body)
    _ -> throwE $ UndefinedStatusCode status body

get :: String -> APIIO BL.ByteString
get url = createReq "GET" url >>= runReq

post :: ToJSON a => String -> a -> APIIO BL.ByteString
post url body = setRequestBodyJSON body <$> createReq "POST" url >>= runReq

-- | Push messages into a receiver. The receiver can be a user, a room or
-- a group, specified by 'ID'.
--
-- A 'Message' represents a message object. For types of the message object,
-- please refer to the <https://developers.line.me/en/docs/messaging-api/reference/#message-objects Send message object>
-- section of the LINE documentation.
--
-- An example usage of 'Message' is like below:
--
-- @
-- messages :: [Message]
-- messages = [ Message $ 'Image' imageURL previewURL
--            , Message $ 'Text' "hello, world!"
--            , Message $ 'Template' "an example template"
--                Confirm "a confirm template"
--                  [ TplMessageAction "ok label" "print this"
--                  , TplURIAction "link label" linkURL
--                  ]
--            ]
-- @
--
-- For more information about the API, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#send-push-message the API reference>.
push :: ID -> [Message] -> APIIO ()
push id' ms = do
  let url = "https://api.line.me/v2/bot/message/push"
  _ <- post url $ object [ "to" .= id'
                         , "messages" .= map toJSON ms
                         ]
  return ()

-- | Send messages to multiple users at any time.
--
-- Messages cannot be sent to groups or rooms.
--
-- For more information, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#send-multicast-messages its API reference>.
multicast :: [ID] -> [Message] -> APIIO ()
multicast ids ms = do
  let url = "https://api.line.me/v2/bot/message/multicast"
  _ <- post url $ object [ "to" .= map toJSON ids
                         , "messages" .= map toJSON ms
                         ]
  return ()

-- | Send messages as a reply to specific webhook event.
--
-- It works similarly to how 'push' does for messages, except that it can only
-- reply through a specific reply token. The token can be obtained from
-- <./Line-Messaging-Webhook-Types.html#t:ReplyableEvent replyable events> on a webhook server.
--
-- For more information, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#send-reply-message its API reference>.
reply :: ReplyToken -> [Message] -> APIIO ()
reply replyToken ms = do
  let url = "https://api.line.me/v2/bot/message/reply"
  _ <- post url $ object [ "replyToken" .= replyToken
                         , "messages" .= map toJSON ms
                         ]
  return ()

-- | Get content body of images, videos and audios sent with
-- <./Line-Messaging-Webhook-Types.html#t:EventMessage event messages>,
-- specified by 'ID'.
--
-- In the event messages, the content body is not included. Users should use
-- 'getContent' to downloaded the content only when it is really needed.
--
-- For more information, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#get-content its API reference>.
getContent :: ID -> APIIO BL.ByteString
getContent id' = do
  let url = concat [ "https://api.line.me/v2/bot/message/"
                   , T.unpack id'
                   , "/content"
                   ]
  get url

-- | Get a profile of a user, specified by 'ID'.
--
-- The user identifier can be obtained via <./Line-Messaging-Webhook-Types.html#t:EventSource EventSource>.
--
-- For more information, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#get-profile its API reference>.
getProfile :: ID -> APIIO Profile
getProfile id' = do
  let url = "https://api.line.me/v2/bot/profile/" ++ T.unpack id'
  bs <- get url
  case eitherDecode' bs of
    Right profile -> return profile
    Left errStr -> lift . throwE . JSONDecodeError $ errStr

-- | Get a profile of a user in a group, specified by the group ID and the user ID.
--
-- Please refer to <https://developers.line.me/en/docs/messaging-api/reference/#get-group-member-profile its API reference>
-- for the difference between this API and 'getProfile'.
getGroupMemberProfile :: ID -> ID -> APIIO Profile
getGroupMemberProfile groupId userId = do
  let url = "https://api.line.me/v2/bot/group/" ++ T.unpack groupId ++ "/member/" ++ T.unpack userId
  bs <- get url
  case eitherDecode' bs of
    Right profile -> return profile
    Left errStr -> lift . throwE . JSONDecodeError $ errStr

-- | Get a profile of a user in a room, specified by the room ID and the user ID.
--
-- Please refer to <https://developers.line.me/en/docs/messaging-api/reference/#get-room-member-profile its API reference>
-- for the difference between this API and 'getProfile'.
getRoomMemberProfile :: ID -> ID -> APIIO Profile
getRoomMemberProfile roomId userId = do
  let url = "https://api.line.me/v2/bot/room/" ++ T.unpack roomId ++ "/member/" ++ T.unpack userId
  bs <- get url
  case eitherDecode' bs of
    Right profile -> return profile
    Left errStr -> lift . throwE . JSONDecodeError $ errStr

data MemberIDs = MemberIDs [ID] (Maybe ContinuationToken)
type ContinuationToken = T.Text

instance FromJSON MemberIDs where
  parseJSON (Object v) = MemberIDs <$> v .: "memberIds" <*> v .:? "next"
  parseJSON _ = fail "ContList"

getMemberIDs :: String -> Maybe T.Text -> ID -> APIIO [ID]
getMemberIDs base token id' = do
  let url = base ++ T.unpack id' ++ "/members/ids" ++ fromMaybe "" (("?start=" ++) . T.unpack <$> token)
  bs <- get url
  case eitherDecode' bs of
    Right (MemberIDs ids Nothing) -> return ids
    Right (MemberIDs ids token') -> (ids ++) <$> getMemberIDs base token' id'
    Left errStr -> lift . throwE . JSONDecodeError $ errStr

-- | Gets the user profile of a member of a group that the bot is in.
--
-- FYI: This feature is only available for LINE@ Approved accounts or official accounts.
--
-- For more information, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#get-group-member-user-ids its API reference>.
getGroupMemberIDs :: ID -> APIIO [ID]
getGroupMemberIDs = getMemberIDs "https://api.line.me/v2/bot/group/" Nothing

-- | Gets the user profile of a member of a room that the bot is in.
--
-- FYI: This feature is only available for LINE@ Approved accounts or official accounts.
--
-- For more information, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#get-room-member-user-ids its API reference>.
getRoomMemberIDs :: ID -> APIIO [ID]
getRoomMemberIDs = getMemberIDs "https://api.line.me/v2/bot/room/" Nothing

leave :: String -> ID -> APIIO ()
leave type' id' = do
  let url = concat [ "https://api.line.me/v2/bot/"
                   , type'
                   , "/"
                   , T.unpack id'
                   , "/leave"
                   ]
  _ <- post url ("" :: T.Text)
  return ()

-- | Leave a room, specified by @'ID'@.
--
-- For more information, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#leave-room its API reference>.
leaveRoom :: ID -> APIIO ()
leaveRoom = leave "room"

-- | Leave a group, specified by @'ID'@.
--
-- For more information, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#leave-group its API reference>.
leaveGroup :: ID -> APIIO ()
leaveGroup = leave "group"
