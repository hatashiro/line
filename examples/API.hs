{-|
An example code to show how to use functions and types for LINE Messaging API.
-}

{-# LANGUAGE OverloadedStrings #-}

module API where

import Line.Messaging.API
import Line.Messaging.Webhook
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- | Message event handler.
--
-- For using the handler and create a webhook server, please refer to the
-- webhook example.
handleMessageEvent :: ReplyableEvent EventMessage -> IO ()
handleMessageEvent event = do
  case getMessage event of
    -- For text event message, just echo
    TextEM _ (Text text) -> echo (getReplyToken event) text
    -- For image, video and audio, download contents
    ImageEM identifier -> download identifier ".jpg"
    VideoEM identifier -> download identifier ".mp4"
    AudioEM identifier -> download identifier ".m4a"
    -- For location, push a template message
    LocationEM identifier location -> pushTemplate identifier location
    _ -> return ()

-- | An unlifter for the 'APIIO' type.
--
-- It uses the 'runAPI' function with a channel access token provided.
api :: APIIO a -> IO (Either APIError a)
api = runAPI $ return "some channel access token"

-- | An echoing action
--
-- It just send a reply with a text. By 'api', the result is
-- 'Either APIError ()', but the error is not handled for simplicity.
echo :: ReplyToken -> T.Text -> IO ()
echo replyToken content = do
  api $ reply replyToken [ Message . Text $ content ]
  return ()

-- | Download content
--
-- It downloads a binary content of image, video or audio spcified by
-- its identifier.
download :: ID -> String -> IO ()
download identifier ext = do
  let filename = "recent" ++ ext
  content <- either (const "") id <$> (api $ getContent identifier)
  BL.writeFile filename content

-- | Push a template message.
--
-- In this case, it is better to use 'reply' instead of 'push', as it is
-- cheap. 'push' is used here for example usage.
pushTemplate :: ID -> Location -> IO ()
pushTemplate identifier (Location _ address _ _) = do
  (name, description, homepage, thumbnail) <- getRestaurant address
  api $ push identifier [
    Message . Text $ "Here are some restaurants around the location.",
    Message . Template "Alt text for old clients" $
      Buttons thumbnail name description [
        TplPostbackAction "Reservation" "Reserved!" name,
        TplURIAction "Homepage" homepage
        ]
    ]
  return ()

  where
  -- | A fucntion to return a list of restaunts for an address
  --
  -- This function is just to be used as an example, so it is not implemented.
  getRestaurant :: T.Text -> IO (T.Text, T.Text, URL, URL)
  getRestaurant address = undefined
