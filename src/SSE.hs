{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module SSE
  ( SSE
  , EventStream
  , EventGet
  , sourceToEventStream
  ) where

import           Data.Aeson
import           Data.ByteString.Builder as BB
import           Data.Text               (Text)
import           Network.HTTP.Media      ((//))
import           Servant

import           Event
import           Server.Types

-- | A content type for Server Sent Events.
data SSE

instance Accept SSE where
  contentType _ = "text" // "event-stream"

eol :: BB.Builder
eol = BB.char8 '\n'

instance MimeRender SSE ServerEvent where
  mimeRender _ (CommentEvent content) =
    BB.toLazyByteString $ ": " <> content <> eol
  mimeRender _ (ServerEvent index event) =
    BB.toLazyByteString $ mconcat
    [ "event: " <> renderEventType event            <> eol
    , "id: "    <> BB.intDec index                  <> eol
    , "data: "  <> BB.lazyByteString (encode event) <> eol
    ]

type EventStream a =
  Headers '[Header "Cache-Control" Text, Header "Connection" Text] (SourceIO a)

type EventGet a =
  StreamGet NewlineFraming SSE (EventStream a)

sourceToEventStream :: SourceIO a -> EventStream a
sourceToEventStream = addHeader "no-cache" . addHeader "keep-alive"
