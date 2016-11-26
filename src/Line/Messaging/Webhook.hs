{-|
This module provides webhook handlers both general and WAI-specific.
-}

module Line.Messaging.Webhook (
  -- * Types
  -- | Re-exported for convenience.
  module Line.Messaging.Webhook.Types,
  -- * Basic webhook
  webhook,
  -- * Webhook as a WAI application
  webhookApp,
  defaultOnFailure,
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Data.Aeson (decode')
import Data.ByteString.Builder (string8)
import Line.Messaging.Webhook.Types
import Line.Messaging.Webhook.Validation (validateSignature)
import Line.Messaging.Types (ChannelSecret)
import Network.HTTP.Types.Status
import Network.Wai

-- | A basic webhook function. It validates a request with a channel secret,
-- and parses the request into a list of webhook events.
--
-- To handle failures, the result is in the form of @'ExceptT' 'WebhookFailure'@.
webhook :: ChannelSecret
        -> Request
        -> ExceptT WebhookFailure IO [Event]
webhook secret req = do
  body <- liftIO $ lazyRequestBody req
  if not $ validateSignature secret req body
  then throwE SignatureVerificationFailed
  else do
    case decode' body of
      Nothing -> throwE MessageDecodeFailed
      Just (Body events) -> return events

waiResponse :: WebhookResult -> Application
waiResponse result req f = case result of
  Ok              -> f $ responseBuilder status200 [] ""
  WaiResponse res -> f res
  WaiApp app      -> app req f

-- | A webhook handler for WAI. It uses 'webhook' internally and returns a WAI
-- 'Application'.
--
-- An example webhook server using WAI will be like below:
--
-- @
-- app :: Application
-- app req f = case pathInfo req of
--   "webhook" : _ -> do
--     secret <- getChannelSecret
--     webhookApp secret handler defaultOnFailure $ req f
--   _ -> undefined
--
-- handler :: [Event] -> IO WebhookResult
-- handler events = forM_ events handleEvent $> Ok
--
-- handleEvent :: Event -> IO ()
-- handleEvent (MessageEvent event) = undefined -- handle a message event
-- handleEvent _ = return ()
-- @
webhookApp :: ChannelSecret -- ^ Channel secret
           -> ([Event] -> IO WebhookResult) -- ^ Event handler
           -> (WebhookFailure -> Application) -- ^ Error handler. Just to return 400 for failures, use 'defaultOnFailure'.
           -> Application
webhookApp secret handler failHandler req f = do
  result <- runExceptT $ webhook secret req
  case result of
    Right events -> handler events >>= waiResponse <*> pure req <*> pure f
    Left exception -> failHandler exception req f

-- | A basic error handler to be used with 'webhookApp'. It returns 400 Bad
-- Request with the 'WebhookFailure' code for its body.
defaultOnFailure :: WebhookFailure -> Application
defaultOnFailure failure _ f = f .
  responseBuilder status400 [] . string8 . show $ failure
