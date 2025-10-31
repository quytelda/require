{-# LANGUAGE OverloadedStrings #-}

module Error
  ( -- * Errors
    GameError(..)
  , gameErrorToServerError
  ) where

import           Control.Exception
import           Data.ByteString.Builder
import           Data.ByteString.Lazy       (LazyByteString)
import qualified Data.Text.Lazy.Builder     as TB
import           Data.Text.Lazy.Encoding    (encodeUtf8Builder)
import           Servant

import           Game.Internal

-- | Game-related Errors
data GameError
  = InvalidPlayer PlayerId -- ^ This 'PlayerId' doesn't exist or conflicts
  | NotEnoughTiles -- ^ The bank doesn't have enough tiles
  | NotEnoughMoney PlayerId -- ^ Insufficient funds for a transaction
  | NotEnoughStock PlayerId Company -- ^ Insufficient stock for a transaction
  | InvalidMove Tile TileZone TileZone -- ^ This tile movement is invalid
  deriving (Eq, Show)

instance Exception GameError

-- | Create a human readable description of a 'GameError'.
describeGameError :: GameError -> LazyByteString
describeGameError = toLazyByteString . describeGameError'
  where
    describeGameError' (InvalidPlayer pid) =
      "invalid player id: "
      <> intDec pid
    describeGameError' NotEnoughTiles =
      "not enough tiles in the drawing pool"
    describeGameError' (NotEnoughMoney pid) =
      renderPid pid
      <> " doesn't have enough money"
    describeGameError' (NotEnoughStock pid com) =
      renderPid pid
      <> " doesn't have enough "
      <> (tl2bs . renderCompany) com
      <> " stock"
    describeGameError' (InvalidMove tile src dst) =
      "cannot move tile "
      <> (tl2bs . renderTile) tile
      <> " from "
      <> (tl2bs . renderTileZone) src
      <> " to "
      <> (tl2bs . renderTileZone) dst

    renderPid 0   = "the bank"
    renderPid pid = "player #" <> intDec pid

    tl2bs = encodeUtf8Builder . TB.toLazyText

gameErrorToServerError :: GameError -> ServerError
gameErrorToServerError err = err400 { errBody = describeGameError err }
