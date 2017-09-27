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
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL

-- | Provided a channel secret, request body and auth signature, it determines
-- the request is properly signatured, which probably means it is sent from a
-- valid LINE server.
--
-- For more details of webhook authentication, please refer to
-- <https://developers.line.me/en/docs/messaging-api/reference/#signature-validation the LINE documentation>.
validateSignature :: ChannelSecret -> BL.ByteString -> Signature -> Bool
validateSignature secret body signature = hash == signature
  where hash = Base64.encode $ hmaclazy (encodeUtf8 secret) body
