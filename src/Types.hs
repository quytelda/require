{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Types
  ( -- * Basic Game Types
    Money
  , PlayerId
  , Tile
  , renderTile
  , TileZone(..)
  , Company(..)

    -- * Game State
  , Stocks
  , GameState(..)
  , defaultGame
  , newGameState
  , Game(..)
  , throwGame
  , GameAction
  , runGameAction
  , withEvent

    -- * Events
  , Event(..)
  , eventSource
  , renderEventType
  , displayEvent
  , ServerEvent(..)

    -- * Errors
  , GameError(..)
  , gameErrorToServerError

    -- * Server Types
  , ServerId
  , ServerState(..)
  , newServerState
  , newPlayerId
    -- ** Event History
  , appendHistory
  , nthEvent
  , sourceHistory
  , logEvents

  -- * Utility Functions
  , textBuilderToJSON
  , putBuilderLn
  ) where

import           Control.Concurrent.STM

import           Error
import           Event
import           Game.Internal
import           Game.Types
import           Server.Types

--------------------------------------------------------------------------------
-- Game State

-- | A game action that generates an event.
type GameAction a = Game (Event, a)

-- | Run an action and publish the event it generates.
runGameAction :: ServerState -> GameAction a -> STM a
runGameAction server action = do
  (event, result) <- runGame action (gameState server)
  appendHistory server event
  return result

-- | Utility function for writing 'GameActions'.
withEvent :: Event -> Game a -> GameAction a
withEvent = fmap . (,)
