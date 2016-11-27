{-|
This module provides a function to check if a webhook request has a correct
signature.
-}

{-# LANGUAGE OverloadedStrings #-}

module Line.Messaging.Webhook.Validation (
  -- * Signature validation
  validateSignature,
  ) where

import Crypto.Hash.SHA256 (hmaclazy)
import Data.Text.Encoding (encodeUtf8)
import Line.Messaging.Types
import Network.Wai
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL

getSignature :: Request -> Maybe B.ByteString
getSignature req = lookup "X-Line-Signature" headers
  where
    headers = requestHeaders req

-- | Provided a channel secret, WAI request and request body, it determines
-- the request is properly signatured, which probably means it is sent
-- from a valid LINE server.
--
-- For more details of webhook authentication, please refer to
-- <https://devdocs.line.me/en/#webhook-authentication the LINE documentation>.
validateSignature :: ChannelSecret -> Request -> BL.ByteString -> Bool
validateSignature secret req body = case getSignature req of
  Nothing -> False
  Just signature -> hash == signature
  where
    hash = Base64.encode $ hmaclazy (encodeUtf8 secret) body
