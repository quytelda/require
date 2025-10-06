module Game where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           System.Random

import           Types

--------------------------------------------------------------------------------
-- Event Handling

doDraw :: PlayerId -> Game Tile
doDraw pid = do
  tile <- grabFromPool >>= maybe (throwError NotEnoughTiles) pure
  setTileStatus tile (Hand pid)
  return tile

doPlay :: PlayerId -> Tile -> Game ()
doPlay pid = moveTile (Hand pid) Play

doDiscard :: PlayerId -> Tile -> Game ()
doDiscard pid = moveTile (Hand pid) Discard

doReturn :: PlayerId -> Tile -> Game ()
doReturn pid = moveTile (Hand pid) Pool

doMarker :: PlayerId -> Company -> Maybe Tile -> Game ()
doMarker _ com mtile = modify' $ \game -> game
  { gameMarkers = Map.alter (const mtile) com (gameMarkers game) }

doMoney :: PlayerId -> Money -> Game ()
doMoney pid amount = do
  bankBal   <- getMoney 0
  playerBal <- getMoney pid

  unless (bankBal >= amount) $
    throwError $ NotEnoughMoney 0

  unless (playerBal >= negate amount) $
    throwError $ NotEnoughMoney pid

  setMoney 0   (bankBal - amount)
  setMoney pid (playerBal + amount)

doStock :: PlayerId -> Company -> Int -> Game ()
doStock pid com amount = do
  bankQty   <- getStock 0   com
  playerQty <- getStock pid com

  unless (bankQty >= amount) $
    throwError $ NotEnoughStock 0 com

  unless (playerQty >= negate amount) $
    throwError $ NotEnoughStock pid com

  setStock 0   com (bankQty - amount)
  setStock pid com (playerQty + amount)

--------------------------------------------------------------------------------
-- Managing Game State

tilesInZone :: TileLoc -> GameState -> [Tile]
tilesInZone zone = Map.keys . Map.filter (== zone) . gameTiles

setTileStatus :: Tile -> TileLoc -> Game ()
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

moveTile :: TileLoc -> TileLoc -> Tile -> Game ()
moveTile fromZone toZone tile = do
  mstatus <- gets $ Map.lookup tile . gameTiles
  case mstatus of
    Just loc | loc == fromZone -> setTileStatus tile toZone
    _                          -> throwError $ InvalidMove tile fromZone toZone

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

--------------------------------------------------------------------------------
-- Utility Functions

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM pre f = do
  p <- pre
  unless p f
