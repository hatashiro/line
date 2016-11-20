module Line.Messaging.API (
  module Line.Messaging.API.Types,
  APIIO,
  runAPI,
  push,
  reply,
  getContent,
  getProfile,
  leaveRoom,
  leaveGroup,
  ) where

import Control.Exception (SomeException(..))
import Control.Lens ((&), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Control.Monad.Trans.Except (runExceptT, ExceptT, throwE, catchE)
import Data.Aeson (ToJSON(..), (.=), object, decode', eitherDecode')
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.API.Types
import Line.Messaging.Types (ChannelAccessToken, ReplyToken)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.Wreq as Wr

type APIIO a = ReaderT ChannelAccessToken (ExceptT APIError IO) a

runAPI :: IO ChannelAccessToken -> APIIO a -> IO (Either APIError a)
runAPI getToken api = getToken >>= runExceptT . runReaderT api

getOpts :: APIIO Wr.Options
getOpts = do
  token <- encodeUtf8 <$> ask
  return $ Wr.defaults & Wr.header "Authorization" .~ [ "Bearer " `B.append` token ]
                       & Wr.checkStatus .~ Just (\ _ _ _ -> Nothing) -- do not throw StatusCodeException

handleError :: SomeException -> (ExceptT APIError IO) a
handleError = throwE . UndefinedError

runReqIO :: IO (Wr.Response BL.ByteString) -> APIIO BL.ByteString
runReqIO reqIO = lift $ do
  res <- liftIO reqIO `catchE` handleError
  let statusCode = res ^. Wr.responseStatus . Wr.statusCode
  let body = res ^. Wr.responseBody
  case statusCode of
    200 -> return $ body
    400 -> throwE $ BadRequest (decode' body)
    401 -> throwE $ Unauthorized (decode' body)
    403 -> throwE $ Forbidden (decode' body)
    429 -> throwE $ TooManyRequests (decode' body)
    500 -> throwE $ InternalServerError (decode' body)
    _ -> throwE $ UndefinedStatusCode statusCode body

get :: String -> APIIO BL.ByteString
get url = do
  opts <- getOpts
  runReqIO $ Wr.getWith opts url

post :: ToJSON a => String -> a -> APIIO BL.ByteString
post url body = do
  opts <- getOpts
  runReqIO $ Wr.postWith opts url (toJSON body)

push :: ID -> [Message] -> APIIO ()
push id' ms = do
  let url = "https://api.line.me/v2/bot/message/push"
  _ <- post url $ object [ "to" .= id'
                         , "messages" .= map toJSON ms
                         ]
  return ()

reply :: ReplyToken -> [Message] -> APIIO ()
reply replyToken ms = do
  let url = "https://api.line.me/v2/bot/message/reply"
  _ <- post url $ object [ "replyToken" .= replyToken
                         , "messages" .= map toJSON ms
                         ]
  return ()

getContent :: ID -> APIIO BL.ByteString
getContent id' = do
  let url = concat [ "https://api.line.me/v2/bot/message/"
                   , T.unpack id'
                   , "/content"
                   ]
  get url

getProfile :: ID -> APIIO Profile
getProfile id' = do
  let url = "https://api.line.me/v2/bot/profile/" ++ T.unpack id'
  bs <- get url
  case eitherDecode' bs of
    Right profile -> return profile
    Left errStr -> lift . throwE . JSONDecodeError $ errStr

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

leaveRoom :: ID -> APIIO ()
leaveRoom = leave "room"

leaveGroup :: ID -> APIIO ()
leaveGroup = leave "group"
