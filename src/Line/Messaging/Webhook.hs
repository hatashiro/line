module Line.Messaging.Webhook (
  module Line.Messaging.Webhook.Types,
  webhook,
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

webhookApp :: ChannelSecret
           -> ([Event] -> IO WebhookResult)
           -> (WebhookFailure -> Application)
           -> Application
webhookApp secret handler failHandler req f = do
  result <- runExceptT $ webhook secret req
  case result of
    Right events -> handler events >>= waiResponse <*> pure req <*> pure f
    Left exception -> failHandler exception req f

defaultOnFailure :: WebhookFailure -> Application
defaultOnFailure failure _ f = f .
  responseBuilder status400 [] . string8 . show $ failure
