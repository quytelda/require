{-# LANGUAGE OverloadedStrings #-}

module Event where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Builder
import           Data.Text                  (Text)
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TBI

import           Game.Internal

-- | Events represents game actions which alter the game state and
-- must be broadcast to all players.
data Event
  = JoinEvent   PlayerId Text -- ^ A new player is joining
  | DrawEvent   PlayerId -- ^ Draw a tile
  | MoveEvent   PlayerId Tile TileZone TileZone -- ^ Move a tile between zones
  | MarkerEvent PlayerId Company (Maybe Tile) -- ^ Place or remove a company marker tile
  | MoneyEvent  PlayerId Money -- ^ Take or return money
  | StockEvent  PlayerId Company Int -- ^ Take or return stocks
  deriving (Eq, Show)

-- | From which player did this event originate?
eventSource :: Event -> PlayerId
eventSource (JoinEvent   pid _)     = pid
eventSource (DrawEvent   pid)       = pid
eventSource (MoveEvent   pid _ _ _) = pid
eventSource (MarkerEvent pid _ _)   = pid
eventSource (MoneyEvent  pid _)     = pid
eventSource (StockEvent  pid _ _)   = pid

renderEventType :: Event -> Builder
renderEventType (JoinEvent   _ _)     = "join"
renderEventType (DrawEvent   _)       = "draw"
renderEventType (MoveEvent   _ _ _ _) = "move"
renderEventType (MarkerEvent _ _ _)   = "marker"
renderEventType (MoneyEvent  _ _)     = "money"
renderEventType (StockEvent  _ _ _)   = "stock"

instance ToJSON Event where
  toJSON (JoinEvent pid name) =
    object [ "type" .= String "join"
           , "source" .= pid
           , "name" .= name
           ]
  toJSON (DrawEvent pid) =
    object [ "type" .= String "draw"
           , "source" .= pid
           ]
  toJSON (MoveEvent pid tile srcZone dstZone) =
    object [ "type" .= String "move"
           , "source" .= pid
           , "tile" .= tile
           , "src" .= srcZone
           , "dst" .= dstZone
           ]
  toJSON (MarkerEvent pid com mtile) =
    object [ "type" .= String "marker"
           , "source" .= pid
           , "company" .= com
           , "tile" .= mtile
           ]
  toJSON (MoneyEvent pid amount) =
    object [ "type" .= String "money"
           , "source" .= pid
           , "amount" .= amount
           ]
  toJSON (StockEvent pid com amount) =
    object [ "type" .= String "stock"
           , "source" .= pid
           , "company" .= com
           , "amount" .= amount
           ]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \obj -> do
    evType <- obj .: "type" :: Parser Text
    pid <- obj .: "source" :: Parser PlayerId
    case evType of
      "join"    -> JoinEvent pid
                   <$> obj .: "name"
      "draw"    -> return $ DrawEvent pid
      "move"    -> MoveEvent pid
                   <$> obj .: "tile"
                   <*> obj .: "src"
                   <*> obj .: "dst"
      "marker"  -> MarkerEvent pid
                   <$> obj .: "company"
                   <*> obj .:? "tile"
      "money"   -> MoneyEvent pid
                   <$> obj .: "amount"
      "stock"   -> StockEvent pid
                   <$> obj .: "company"
                   <*> obj .: "amount"
      _         -> fail "invalid event type"

displayEvent :: Event -> TB.Builder
displayEvent event =
  "Player " <> TBI.decimal (eventSource event) <> ": "
  <> displayEvent' event
  where
    displayEvent' (JoinEvent   _ name) =
      "JOIN as \"" <> TB.fromText name <> "\""
    displayEvent' (DrawEvent   _) = "DRAW"
    displayEvent' (MoveEvent   _ tile src dst) =
      "MOVE " <> renderTile tile
      <> " from " <> renderTileZone src
      <> " to " <> renderTileZone dst
    displayEvent' (MarkerEvent _ com mtile) =
      "MARKER for " <> renderCompany com
      <> case mtile of
           Just tile -> " to " <> renderTile tile
           Nothing   -> " removed"
    displayEvent' (MoneyEvent  _ amount) =
      "MONEY transfer of " <> TBI.decimal amount
    displayEvent' (StockEvent  _ com amount) =
      "STOCK transfer of " <> TBI.decimal amount
      <> " " <> renderCompany com
