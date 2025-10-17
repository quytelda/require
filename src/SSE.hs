{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module SSE where

import           Data.Aeson
import           Data.Text          (Text)
import           Network.HTTP.Media ((//))
import           Servant

-- | A content type for Server Sent Events.
data SSE

instance Accept SSE where
  contentType _ = "text" // "event-stream"

instance ToJSON a => MimeRender SSE a where
  mimeRender _ a = "data: " <> encode a <> "\n"

type EventStream a =
  Headers '[Header "Cache-Control" Text, Header "Connection" Text] (SourceIO a)

type EventGet a =
  StreamGet NewlineFraming SSE (EventStream a)

sourceToEventStream :: SourceIO a -> EventStream a
sourceToEventStream = addHeader "no-cache" . addHeader "keep-alive"
