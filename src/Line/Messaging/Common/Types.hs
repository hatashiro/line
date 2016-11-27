{-|
This module is to define aliases commonly used in other modules.
-}

module Line.Messaging.Common.Types (
  -- * General types
  ID,
  URL,
  Postback,
  -- * LINE API types
  ChannelSecret,
  ChannelAccessToken,
  ) where

import qualified Data.Text as T

-- | A type alias to specify an identifier of something.
type ID = T.Text
-- | A type alias to specify a URL.
type URL = T.Text
-- | A type alias for postback data.
type Postback = T.Text

-- | A type alias to specify a channel secret. About issueing and using the
-- channel secret, please refer to corresponding LINE documentations.
type ChannelSecret = T.Text
-- | A type alias to specify a channel access token. About issueing and using
-- the channel access token, please refer to corresponding LINE documentations.
type ChannelAccessToken = T.Text
