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
  -- * Webhook as a Scotty action
  webhookAction,
  defaultOnFailure',
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Data.Aeson (decode')
import Data.ByteString.Builder (string8)
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.Webhook.Types
import Line.Messaging.Webhook.Validation (validateSignature)
import Network.HTTP.Types.Status
import Network.Wai
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scotty

-- | A basic webhook function. It validates a request with a channel secret,
-- signature and body, and parses the body into a list of webhook events.
--
-- To handle failures, the result is in the form of @'ExceptT' 'WebhookFailure'@.
webhook :: (Monad m)
        => ChannelSecret
        -> BL.ByteString -- ^ Request body
        -> Signature -- ^ Auth signature in @X-Line-Signature@ header
        -> ExceptT WebhookFailure m [Event]
webhook secret body sig = do
  if not $ validateSignature secret body sig
    then throwE SignatureVerificationFailed
    else do
      case decode' body of
        Nothing -> throwE MessageDecodeFailed
        Just (Body events) -> return events

getSigFromWaiReq :: Request -> Maybe Signature
getSigFromWaiReq = lookup "X-Line-Signature" . requestHeaders

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
--     webhookApp secret handler defaultOnFailure req f
--   _ -> undefined
--
-- handler :: [Event] -> IO ()
-- handler events = forM_ events handleEvent
--
-- handleEvent :: Event -> IO ()
-- handleEvent (MessageEvent event) = undefined -- handle a message event
-- handleEvent _ = return ()
-- @
webhookApp :: ChannelSecret -- ^ Channel secret
           -> ([Event] -> IO ()) -- ^ Event handler
           -> (WebhookFailure -> Application)
              -- ^ Error handler. Just to return 400 for failures, use 'defaultOnFailure'.
           -> Application
webhookApp secret handler failHandler req f = do
  body <- lazyRequestBody req
  let maybeSig = getSigFromWaiReq req
  case maybeSig of
    Nothing -> failHandler SignatureVerificationFailed req f
    Just sig -> do
      result <- runExceptT $ webhook secret body sig
      case result of
        Right events -> handler events >> (f $ responseBuilder status200 [] "")
        Left exception -> failHandler exception req f

-- | A basic error handler to be used with 'webhookApp'. It returns 400 Bad
-- Request with the 'WebhookFailure' code for its body.
defaultOnFailure :: WebhookFailure -> Application
defaultOnFailure failure _ f = f .
  responseBuilder status400 [] . string8 . show $ failure

-- | A webhook handler for Scotty. It uses 'webhook' internally and returns a
-- Scotty action of type 'ActionM' @()@
--
-- An example webhook server using WAI will be like below:
--
-- @
-- main :: IO ()
-- main = scotty 3000 $ do
--   post "/webhook" $ webhookAction handler defaultOnFailure'
--
-- handler :: [Event] -> IO ()
-- handler events = forM_ events handleEvent
--
-- handleEvent :: Event -> IO ()
-- handleEvent (MessageEvent event) = undefined -- handle a message event
-- handleEvent _ = return ()
-- @
webhookAction :: ChannelSecret -- ^ Channel secret
              -> ([Event] -> IO ()) -- ^ Event handler
              -> (WebhookFailure -> Scotty.ActionM ())
                 -- ^ Error handler. Just to return 400 for failures, use 'defaultOnFailure''.
              -> Scotty.ActionM ()
webhookAction secret handler failHandler = do
  body <- Scotty.body
  maybeSig <- Scotty.header "X-Line-Signature"
  case maybeSig of
    Nothing -> failHandler SignatureVerificationFailed
    Just sig -> do
      result <- runExceptT $ webhook secret body (encodeUtf8 $ TL.toStrict sig)
      case result of
        Right events -> (liftIO $ handler events) >> Scotty.text ""
        Left exception -> failHandler exception

-- | A basic error handler to be used with 'webhookAction'. It returns 400 Bad
-- Request with the 'WebhookFailure' code for its body. It has the same purpose
-- as 'defaultOnFailure', except that it is for Scotty.
defaultOnFailure' :: WebhookFailure -> Scotty.ActionM ()
defaultOnFailure' err = do
  Scotty.status status400
  Scotty.text . TL.pack . show $ err
