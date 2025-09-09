{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import           Conduit
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           System.Random

import           Types

handleEvent :: MonadThrow m => Event -> GameMonad m Event
handleEvent event@(JoinEvent pid)             = do exists <- playerExists pid
                                                   when exists $
                                                     throwM $ BadPlayerId pid
                                                   addPlayer pid
                                                   return event
handleEvent (DrawEvent pid _)                 = do tile <- grabFromPool
                                                   setTileStatus (Hand pid) tile
                                                   return $
                                                     DrawEvent pid (Just tile)
handleEvent event@(PlayEvent pid tile)        = checkHasTile pid tile
                                                *> setTileStatus Play tile
                                                *> pure event
handleEvent event@(DiscardEvent pid tile)     = checkHasTile pid tile
                                                *> setTileStatus Discard tile
                                                *> pure event
handleEvent event@(ReturnEvent pid tile)      = checkHasTile pid tile
                                                *> setTileStatus Pool tile
                                                *> pure event
handleEvent event@(MarkerEvent _ com mtile)   = setMarker com mtile
                                                *> pure event
handleEvent event@(MoneyEvent pid amount)     = transferMoney pid amount
                                                *> pure event
handleEvent event@(StockEvent pid com amount) = transferStock pid com amount
                                                *> pure event

--------------------------------------------------------------------------------
-- Game Transactions

playerExists :: MonadThrow m => PlayerId -> GameMonad m Bool
playerExists pid = do
  Game{..} <- get
  return $ Map.member pid gameMoney || Map.member pid gameStocks

addPlayer :: Monad m => PlayerId -> GameMonad m ()
addPlayer pid = modify' $ \game -> game
  { gameMoney = Map.insert pid 0 $ gameMoney game
  , gameStocks = Map.insert pid Map.empty $ gameStocks game
  }

grabFromPool :: MonadThrow m => GameMonad m Coord
grabFromPool = do
  pool <- gets $ Map.keys . Map.filter (== Pool) . gameTiles
  (n, g) <- uniformR (0, length pool - 1) <$> gets gameRNG
  if null pool
    then throwM OutOfTiles
    else modify' (\game -> game { gameRNG = g })
         *> pure (pool !! n)

getMarker :: Monad m => Company -> GameMonad m (Maybe Coord)
getMarker com = gets $ Map.lookup com . gameMarkers

setMarker :: Monad m => Company -> Maybe Coord -> GameMonad m ()
setMarker com mtile = modify' $ \game -> game
  { gameMarkers = Map.alter (const mtile) com (gameMarkers game) }

getTileStatus :: Monad m => Coord -> GameMonad m TileLoc
getTileStatus tile = gets $ Map.findWithDefault Pool tile . gameTiles

setTileStatus :: Monad m => TileLoc -> Coord -> GameMonad m ()
setTileStatus status tile = modify' $ \game ->
  game { gameTiles = Map.insert tile status (gameTiles game) }

playerHasTile :: Monad m => Coord -> PlayerId -> GameMonad m Bool
playerHasTile tile pid = gets $
  maybe False (== Hand pid)
  . Map.lookup tile
  . gameTiles

checkHasTile :: MonadThrow m => PlayerId -> Coord -> GameMonad m ()
checkHasTile pid tile = do
  hasTile <- gets
    $ maybe False (== Hand pid)
    . Map.lookup tile
    . gameTiles

  unless hasTile $
    throwM $ MissingTile pid tile

getMoney :: MonadThrow m => PlayerId -> GameMonad m Money
getMoney pid =
  gets (Map.lookup pid . gameMoney)
  >>= maybe (throwM $ BadPlayerId pid) return

setMoney :: MonadThrow m => PlayerId -> Money -> GameMonad m ()
setMoney pid amount = do
  exists <- playerExists pid
  unless exists $
    throwM $ BadPlayerId pid

  modify' $ \game ->
    game { gameMoney = Map.insert pid amount (gameMoney game) }

getStocks :: MonadThrow m => PlayerId -> Company -> GameMonad m Int
getStocks pid com = do
  mstocks <- gets $ Map.lookup pid . gameStocks
  case mstocks of
    Nothing     -> throwM $ BadPlayerId pid
    Just stocks -> return $ Map.findWithDefault 0 com stocks

setStocks :: MonadThrow m => PlayerId -> Company -> Int -> GameMonad m ()
setStocks pid com qty = do
  stocks <- gets (Map.lookup pid . gameStocks)
            >>= maybe (throw $ BadPlayerId pid) pure
  let stocks' = Map.insert com qty stocks
  modify' $ \game ->
    game { gameStocks = Map.insert pid stocks' (gameStocks game) }

transferMoney :: MonadThrow m => PlayerId -> Money -> GameMonad m ()
transferMoney pid amount = do
  bankBal   <- getMoney 0
  playerBal <- getMoney pid

  unless (bankBal >= amount) $
    throwM $ OutOfMoney 0

  unless (playerBal >= negate amount) $
    throwM $ OutOfMoney pid

  setMoney 0 (bankBal - amount)
  setMoney pid (playerBal + amount)

transferStock :: MonadThrow m => PlayerId -> Company -> Int -> GameMonad m ()
transferStock pid com amount = do
  bankQty   <- getStocks 0   com
  playerQty <- getStocks pid com

  unless (bankQty >= amount) $
    throwM $ OutOfStock 0 com

  unless (playerQty >= negate amount) $
    throwM $ OutOfStock pid com

  setStocks 0   com (bankQty - amount)
  setStocks pid com (playerQty + amount)
