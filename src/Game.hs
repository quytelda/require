{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import           Conduit
import           Control.Monad
import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           System.Random

import           Types

--------------------------------------------------------------------------------
-- Event Handling

handleEvent :: MonadThrow m => Event -> Game m Event
handleEvent event@(JoinEvent pid)             = pure event <* handleJoin pid
handleEvent       (DrawEvent pid _)           = handleDraw pid
handleEvent event@(PlayEvent pid tile)        = pure event <* handlePlay pid tile
handleEvent event@(DiscardEvent pid tile)     = pure event <* handleDiscard pid tile
handleEvent event@(ReturnEvent pid tile)      = pure event <* handleReturn pid tile
handleEvent event@(MarkerEvent pid com tile)  = pure event <* handleMarker pid com tile
handleEvent event@(MoneyEvent pid amount)     = pure event <* handleMoney pid amount
handleEvent event@(StockEvent pid com amount) = pure event <* handleStock pid com amount

handleJoin :: MonadThrow m => PlayerId -> Game m ()
handleJoin _ = return ()

handleDraw :: MonadThrow m => PlayerId -> Game m Event
handleDraw pid = do
  tile <- grabFromPool >>= maybe (throwM NotEnoughTiles) pure
  setTileStatus tile (Hand pid)
  return $ DrawEvent pid (Just tile)

handleTileMove
  :: MonadThrow m
  => TileLoc
  -> PlayerId
  -> Tile
  -> Game m ()
handleTileMove zone pid tile = do
  unlessM (playerHasTile pid tile) $
    throwM $ MissingTile pid tile
  setTileStatus tile zone

handlePlay :: MonadThrow m => PlayerId -> Tile -> Game m ()
handlePlay = handleTileMove Play

handleDiscard :: MonadThrow m => PlayerId -> Tile -> Game m ()
handleDiscard = handleTileMove Discard

handleReturn :: MonadThrow m => PlayerId -> Tile -> Game m ()
handleReturn = handleTileMove Pool

handleMarker :: Monad m => PlayerId -> Company -> Maybe Tile -> Game m ()
handleMarker _ com mtile = modify' $ \game -> game
  { gameMarkers = Map.alter (const mtile) com (gameMarkers game) }

handleMoney :: MonadThrow m => PlayerId -> Money -> Game m ()
handleMoney pid amount = do
  bankBal   <- getMoney 0
  playerBal <- getMoney pid

  unless (bankBal >= amount) $
    throwM $ NotEnoughMoney 0

  unless (playerBal >= negate amount) $
    throwM $ NotEnoughMoney pid

  setMoney 0   (bankBal - amount)
  setMoney pid (playerBal + amount)

handleStock :: MonadThrow m => PlayerId -> Company -> Int -> Game m ()
handleStock pid com amount = do
  bankQty   <- getStock 0   com
  playerQty <- getStock pid com

  unless (bankQty >= amount) $
    throwM $ NotEnoughStock 0 com

  unless (playerQty >= negate amount) $
    throwM $ NotEnoughStock pid com

  setStock 0   com (bankQty - amount)
  setStock pid com (playerQty + amount)

--------------------------------------------------------------------------------
-- Managing Game State

setTileStatus :: Monad m => Tile -> TileLoc -> Game m ()
setTileStatus tile status = modify' $ \game ->
  game { gameTiles = Map.insert tile status (gameTiles game) }

grabFromPool :: MonadThrow m => Game m (Maybe Tile)
grabFromPool = do
  pool <- gets $ Map.keys . Map.filter (== Pool) . gameTiles
  (n, g) <- uniformR (0, length pool - 1) <$> gets gameRNG
  if null pool
    then pure Nothing
    else modify' (\game -> game { gameRNG = g })
         *> pure (Just (pool !! n))

playerHasTile :: Monad m => PlayerId -> Tile -> Game m Bool
playerHasTile pid tile = gets
  $ maybe False (== Hand pid)
  . Map.lookup tile
  . gameTiles

getMoney :: Monad m => PlayerId -> Game m Money
getMoney pid =
  gets (Map.findWithDefault 0 pid . gameMoney)

setMoney :: Monad m => PlayerId -> Money -> Game m ()
setMoney pid amount =
  modify' $ \game ->
    game { gameMoney = Map.insert pid amount (gameMoney game) }

getStock :: Monad m => PlayerId -> Company -> Game m Int
getStock pid com = do
  stocks <- gets gameStocks
  return $
    maybe 0 id (Map.lookup pid stocks >>= Map.lookup com)

setStock :: Monad m => PlayerId -> Company -> Int -> Game m ()
setStock pid com qty = do
  stocks <- gets (Map.findWithDefault mempty pid . gameStocks)
  let stocks' = Map.insert com qty stocks
  modify' $ \game ->
    game { gameStocks = Map.insert pid stocks' (gameStocks game) }

--------------------------------------------------------------------------------
-- Utility Functions

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM pre f = do
  p <- pre
  unless p f
