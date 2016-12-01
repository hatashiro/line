{-|
An example code to show how to create a simple WAI application to handle LINE
message webhooks.
-}

{-# LANGUAGE OverloadedStrings #-}

module WAIWebhook where

import Control.Monad (forM_)
import Line.Messaging.Webhook
import Line.Messaging.Types (Text(..), Location(..))
import Network.Wai
import qualified Data.Text as T

-- | An IO action to get channel secret. The channel secret is issued for each
-- LINE bot, and can be found in the LINE developer page.
getChannelSecret :: IO ChannelSecret
getChannelSecret = return "some channel secret"

-- | A WAI application to handle webhook requests.
--
-- 'webhookApp' is used with a channel secret, event handler and error handler.
app :: Application
app req f = do
  channelSecret <- getChannelSecret
  webhookApp channelSecret handler defaultOnFailure req f

-- | An event handler.
--
-- A webhook request can contain several events at once, so the first argument
-- is a list of 'Event's. The function just call the event handler for each
-- event.
handler :: [Event] -> IO ()
handler events = forM_ events handleEvent

-- | A function to handle each event.
--
-- The type of events can be decided with pattern matching. About possible
-- types, please refer to the doc of Line.Messaging.Webhook.Types module.
--
-- Basically, content of each event has a type of 'EventTuple', which may or
-- may not contain a reply token and internal data.
handleEvent :: Event -> IO ()
handleEvent (JoinEvent event) = printSource event
handleEvent (LeaveEvent event) = printSource event
handleEvent (MessageEvent event) = handleMessageEvent event
handleEvent _ = return ()

-- | An example function to print event source of event.
--
-- Event source exists for all the event tuple types, so the reply token and
-- data types can be polymorphic.
printSource :: EventTuple r a -> IO ()
printSource event = do
  putStrLn . concatMap T.unpack $ case getSource event of
    User identifier -> [ "A user", identifier ]
    Group identifier -> [ "A group", identifier ]
    Room identifier -> [ "A room", identifier ]

-- | An example function to print text and location details of a message
--
-- A message event is always replyable and has message data in it.
handleMessageEvent :: ReplyableEvent EventMessage -> IO ()
handleMessageEvent event = do
  putStrLn . concatMap T.unpack $ case getMessage event of
    TextEM identifier (Text text) -> -- TextEM stands for a text event message
      -- print message ID and internal text
      [ "A text message", identifier, text ]
    LocationEM identifier (Location title address _ _) ->
      -- print message ID and location information
      [ "A location message", identifier, title, address ]
    _ -> []
