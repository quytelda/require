{-# LANGUAGE RecordWildCards #-}
module Types where

import           Conduit
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Conduit.Lift
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           System.Random

type Money = Int
type PlayerId = Int

-- | Tile coordinates: the column is an integer between 1 and 12 and
-- the row is a character between 'A' and 'I'.
type Coord = (Int, Char)

-- | Which game zone is a tile currently in?
data TileLoc
  = Pool
  | Hand PlayerId
  | Play
  | Discard
  deriving (Eq, Show)

-- | Companies in which players may invest
data Company
  = Triangle
  | Love
  | Albanian
  | Fiesta
  | Wonder
  | Centennial
  | Imperious
  deriving (Enum, Eq, Show, Ord)

type Stocks = Map Company Int

type GameMonad = StateT Game

execGameMonad
  :: Monad m
  => Game
  -> ConduitT i o (GameMonad m) r
  -> ConduitT i o m Game
execGameMonad = execStateLC

evalGameMonad
  :: Monad m
  => Game
  -> ConduitT i o (GameMonad m) r
  -> ConduitT i o m r
evalGameMonad = evalStateLC

-- | Game State
data Game = Game
  { gameTiles   :: Map Coord TileLoc
  , gameMarkers :: Map Company Coord
  , gameMoney   :: Map PlayerId Money
  , gameStocks  :: Map PlayerId Stocks
  , gameRNG     :: StdGen
  } deriving (Eq, Show)

defaultGame :: Game
defaultGame = Game
  { gameTiles = Map.fromList $ zip allTiles (repeat Pool)
  , gameMarkers = Map.empty
  , gameMoney = Map.singleton 0 242000 -- 60*100 + 40*500 + 36*1000 + 36*5000
  , gameStocks = Map.singleton 0 defaultBankStocks
  , gameRNG = mkStdGen 0
  }
  where
    defaultBankStocks = Map.fromList $ zip [Triangle ..] (repeat 25)
    allTiles = [(col, row) | col <- [1..12], row <- ['A'..'I']]

newGame :: MonadIO m => m Game
newGame = do
  gen <- getStdGen
  return defaultGame { gameRNG = gen }

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
  { gameMarkers = Map.update (const mtile) com (gameMarkers game) }

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
  playerExists <- gets $ Map.member pid . gameMoney
  unless playerExists $
    throwM $ BadPlayerId pid

  modify' $ \game ->
    game { gameMoney = Map.insert pid amount (gameMoney game) }

getStocks :: MonadThrow m => PlayerId -> Company -> GameMonad m Int
getStocks pid com = do
  mstocks <- gets $ Map.lookup pid . gameStocks
  case mstocks of
    Nothing -> throwM $ BadPlayerId pid
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

  unless (bankBal > amount) $
    throwM $ OutOfMoney 0

  unless (playerBal > negate amount) $
    throwM $ OutOfMoney pid

  setMoney 0 (bankBal - amount)
  setMoney pid (playerBal + amount)

transferStock :: MonadThrow m => PlayerId -> Company -> Int -> GameMonad m ()
transferStock pid com amount = do
  bankQty   <- getStocks 0   com
  playerQty <- getStocks pid com

  unless (bankQty > amount) $
    throwM $ OutOfStock 0 com

  unless (playerQty > negate amount) $
    throwM $ OutOfStock pid com

  setStocks 0   com (bankQty - amount)
  setStocks pid com (playerQty + amount)

data Event
  = HelloEvent   PlayerId -- ^ Inform a new client of their player ID
  | JoinEvent    PlayerId -- ^ A new player joins the game
  | DrawEvent    PlayerId (Maybe Coord) -- ^ Draw a tile
  | PlayEvent    PlayerId Coord -- ^ Put a tile on the board
  | DiscardEvent PlayerId Coord -- ^ Discard an unusable tile
  | ReturnEvent  PlayerId Coord -- ^ Return a pile to the pool
  | MarkerEvent  PlayerId Company (Maybe Coord) -- ^ Place or remove a company marker tile
  | MoneyEvent   PlayerId Money -- ^ Take or return money
  | StockEvent   PlayerId Company Int -- ^ Take or return stocks
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Errors

data GameError
  = BadPlayerId PlayerId -- ^ Player ID does not exist
  | OutOfMoney  PlayerId
  | OutOfStock  PlayerId Company
  | OutOfTiles
  | MissingTile PlayerId Coord -- ^ This player doesn't have this tile
  deriving (Eq, Show)

instance Exception GameError
