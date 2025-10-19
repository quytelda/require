{-# LANGUAGE OverloadedStrings #-}

module Game
  ( -- * Event Handling
    doJoin
  , doDraw
  , doMove
  , doMarker
  , doMoney
  , doStock

    -- * Game State Helpers
  , tilesInZone
  , setTileStatus
  , grabFromPool
  , getHand
  , getMoney
  , setMoney
  , getStock
  , setStock
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Functor
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           Data.Text           (Text)
import           System.Random

import           Types

--------------------------------------------------------------------------------
-- Event Handling

doJoin :: PlayerId -> Text -> GameAction ()
doJoin pid name = withEvent (JoinEvent pid name) $ do
  exists <- isValidPlayer pid
  when exists $
    throwGame $ InvalidPlayer pid

  modify' $ \game -> game { gamePlayers = IntMap.insert pid name (gamePlayers game) }

doDraw :: PlayerId -> GameAction Tile
doDraw pid = withEvent (DrawEvent pid) $ do
  checkPlayerId pid
  tile <- grabFromPool >>= maybe (throwGame NotEnoughTiles) pure
  setTileStatus tile (Hand pid)
  return tile

doMove :: PlayerId -> TileZone -> TileZone -> Tile -> GameAction ()
doMove pid fromZone toZone tile = withEvent (MoveEvent pid tile fromZone toZone) $ do
  checkPlayerId pid
  mstatus <- gets $ Map.lookup tile . gameTiles
  case mstatus of
    Just loc | loc == fromZone -> setTileStatus tile toZone
    _                          -> throwGame $ InvalidMove tile fromZone toZone

doMarker :: PlayerId -> Company -> Maybe Tile -> GameAction ()
doMarker pid com mtile = withEvent (MarkerEvent pid com mtile) $ do
  checkPlayerId pid
  modify' $ \game ->
    game { gameMarkers = Map.alter (const mtile) com (gameMarkers game) }

doMoney :: PlayerId -> Money -> GameAction ()
doMoney pid amount = withEvent (MoneyEvent pid amount) $ do
  checkPlayerId pid
  bankBal   <- getMoney 0
  playerBal <- getMoney pid

  unless (bankBal >= amount) $
    throwGame $ NotEnoughMoney 0

  unless (playerBal >= negate amount) $
    throwGame $ NotEnoughMoney pid

  setMoney 0   (bankBal - amount)
  setMoney pid (playerBal + amount)

doStock :: PlayerId -> Company -> Int -> GameAction ()
doStock pid com amount = withEvent (StockEvent pid com amount) $ do
  checkPlayerId pid
  bankQty   <- getStock 0   com
  playerQty <- getStock pid com

  unless (bankQty >= amount) $
    throwGame $ NotEnoughStock 0 com

  unless (playerQty >= negate amount) $
    throwGame $ NotEnoughStock pid com

  setStock 0   com (bankQty - amount)
  setStock pid com (playerQty + amount)

--------------------------------------------------------------------------------
-- Managing Game State

isValidPlayer :: PlayerId -> Game Bool
isValidPlayer pid = gets $ IntMap.member pid . gamePlayers

checkPlayerId :: PlayerId -> Game ()
checkPlayerId pid = do
  valid <- isValidPlayer pid
  unless valid $
    throwGame $ InvalidPlayer pid

tilesInZone :: TileZone -> GameState -> [Tile]
tilesInZone zone = Map.keys . Map.filter (== zone) . gameTiles

setTileStatus :: Tile -> TileZone -> Game ()
setTileStatus tile status = modify' $ \game ->
  game { gameTiles = Map.insert tile status (gameTiles game) }

grabFromPool :: Game (Maybe Tile)
grabFromPool = do
  pool <- gets $ Map.keys . Map.filter (== Pool) . gameTiles
  (n, g) <- gets $ uniformR (0, length pool - 1) . gameRNG
  if null pool
    then pure Nothing
    else modify' (\game -> game { gameRNG = g })
         $> Just (pool !! n)

getHand :: PlayerId -> Game [Tile]
getHand pid = do
  tileMap <- gets gameTiles
  return $ Map.keys $ Map.filter (== Hand pid) tileMap

getMoney :: PlayerId -> Game Money
getMoney pid =
  gets (Map.findWithDefault 0 pid . gameMoney)

setMoney :: PlayerId -> Money -> Game ()
setMoney pid amount =
  modify' $ \game ->
    game { gameMoney = Map.insert pid amount (gameMoney game) }

getStock :: PlayerId -> Company -> Game Int
getStock pid com = do
  stocks <- gets gameStocks
  return $
    fromMaybe 0 (Map.lookup pid stocks >>= Map.lookup com)

setStock :: PlayerId -> Company -> Int -> Game ()
setStock pid com qty = do
  stocks <- gets (Map.findWithDefault mempty pid . gameStocks)
  let stocks' = Map.insert com qty stocks
  modify' $ \game ->
    game { gameStocks = Map.insert pid stocks' (gameStocks game) }
