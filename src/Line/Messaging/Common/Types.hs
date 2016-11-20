module Line.Messaging.Common.Types (
  ID,
  URL,
  ChannelSecret,
  ChannelAccessToken,
  ) where

import qualified Data.Text as T

type ID = T.Text
type URL = T.Text

type ChannelSecret = T.Text
type ChannelAccessToken = T.Text
