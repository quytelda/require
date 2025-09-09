{-# LANGUAGE RecordWildCards #-}
module Types where

import           Conduit
import           Control.Exception
import           Control.Monad.State
import           Data.ByteString     (ByteString)
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

-- | Create a default new game with a random RNG seed.
newGame :: MonadIO m => m Game
newGame = do
  gen <- getStdGen
  return defaultGame { gameRNG = gen }

data Event
  = JoinEvent    PlayerId       -- ^ A new player joining
  | DrawEvent    PlayerId (Maybe Coord) -- ^ Draw a tile
  | PlayEvent    PlayerId Coord -- ^ Put a tile on the board
  | DiscardEvent PlayerId Coord -- ^ Discard an unusable tile
  | ReturnEvent  PlayerId Coord -- ^ Return a pile to the pool
  | MarkerEvent  PlayerId Company (Maybe Coord) -- ^ Place or remove a company marker tile
  | MoneyEvent   PlayerId Money -- ^ Take or return money
  | StockEvent   PlayerId Company Int -- ^ Take or return stocks
  deriving (Eq, Show)

eventSource :: Event -> PlayerId
eventSource (JoinEvent    pid)     = pid
eventSource (DrawEvent    pid _)   = pid
eventSource (PlayEvent    pid _)   = pid
eventSource (DiscardEvent pid _)   = pid
eventSource (ReturnEvent  pid _)   = pid
eventSource (MarkerEvent  pid _ _) = pid
eventSource (MoneyEvent   pid _)   = pid
eventSource (StockEvent   pid _ _) = pid

--------------------------------------------------------------------------------
-- Errors

data ErrorMessage = ErrorMessage Event ByteString
  deriving (Eq, Show)

-- | Problematic situations that might arise during the game.
data GameError
  = BadPlayerId PlayerId -- ^ Player ID does not exist
  | OutOfMoney  PlayerId
  | OutOfStock  PlayerId Company
  | OutOfTiles
  | MissingTile PlayerId Coord -- ^ This player doesn't have this tile
  deriving (Eq, Show)

instance Exception GameError
