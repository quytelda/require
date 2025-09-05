{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import           Conduit
import           Control.Concurrent.STM
import           Control.Exception
import           Data.Conduit.Network
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Protocol
import           Types

handleEvent :: MonadThrow m => Game -> Event -> m Game
handleEvent game (DrawEvent pid _)           = case grabFromPool game of
                                                 Just (tile, game') ->
                                                   pure $ setTileStatus tile (Hand pid) game'
                                                 Nothing -> throwM OutOfTiles
handleEvent game (PlayEvent pid tile)        = if playerHasTile tile pid game
                                               then pure $ setTileStatus tile Play game
                                               else throwM $ MissingTile pid tile
handleEvent game (DiscardEvent pid tile)     = if playerHasTile tile pid game
                                               then pure $ setTileStatus tile Discard game
                                               else throwM $ MissingTile pid tile
handleEvent game (ReturnEvent pid tile)      = if playerHasTile tile pid game
                                               then pure $ setTileStatus tile Pool game
                                               else throwM $ MissingTile pid tile
handleEvent game (MarkerEvent _ com mtile)   = pure $ setMarker com mtile game
handleEvent game (MoneyEvent pid amount)     = transferMoney pid amount game
handleEvent game (StockEvent pid com amount) = transferStock pid com amount game
handleEvent game _                           = pure game
