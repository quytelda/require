module Types where

import           Control.Exception
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map

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

-- | The collection of scorable assets held by a player (or the bank)
data Assets = Assets
  { moneyAssets :: Money
  , stockAssets :: Map Company Int
  } deriving (Eq, Show)

defaultAssets :: Assets
defaultAssets = Assets 0 $ Map.fromList $ zip [Triangle ..] (repeat 0)

-- | The contents of the bank before distributing starting assets,
-- i.e. the collection of all assets in the game.
defaultBankAssets :: Assets
defaultBankAssets = Assets
  { moneyAssets = 242000 -- 60*100 + 40*500 + 36*1000 + 36*5000
  , stockAssets = Map.fromList $ zip [Triangle ..] (repeat 25)
  }

-- | Game State
data Game = Game
  { gameTiles        :: Map Coord TileLoc
  , gameBank         :: Assets
  , gamePlayerAssets :: Map PlayerId Assets
  } deriving (Eq, Show)

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
  | MissingTile PlayerId Coord -- ^ This player doesn't have this tile
  deriving (Eq, Show)

instance Exception GameError
