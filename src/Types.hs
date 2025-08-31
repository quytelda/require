module Types where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
  | Impose
  deriving (Enum, Eq, Show, Ord)

-- | The collection of scorable assets held by a player (or the bank)
data Assets = Assets
  { moneyAssets :: Money
  , stockAssets :: Map Company Int
  } deriving (Eq, Show)

-- | The contents of the bank before distributing starting assets,
-- i.e. the collection of all assets in the game.
defaultBankAssets :: Assets
defaultBankAssets = Assets
  { moneyAssets = 242000 -- 60*100 + 40*500 + 36*1000 + 36*5000
  , stockAssets = Map.fromList $ zip [Triangle ..] (repeat 25)
  }

data Action
  = DrawAction (Maybe Coord) -- ^ Draw a tile
  | PlayAction Coord -- ^ Put a tile on the board
  | DiscardAction Coord -- ^ Discard an unusable tile
  | ReturnAction Coord -- ^ Return a pile to the pool
  | MarkerAction (Maybe Coord) -- ^ Place or remove a company marker tile
  | MoneyAction Money -- ^ Take or return money
  | StockAction Company Int -- ^ Take or return stocks
  deriving (Eq, Show)

-- | An event represents something happening in the game that needs to
-- be shared with all players.
data Event = Event PlayerId Event
  deriving (Eq, Show)
