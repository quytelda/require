{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Types where

import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Read         as Read
import           GHC.Generics
import           System.Random

--------------------------------------------------------------------------------
-- Basic Game Types

type Money = Int
type PlayerId = Int

-- | Tile coordinates (e.g. "9F"): the column is an integer between 1
-- and 12 (inclusive) and the row is a character between 'A' and 'I'
-- (inclusive).
data Tile = Tile !Int !Char
  deriving (Eq, Ord, Show)

instance ToJSON Tile where
  toJSON (Tile col row) = String $ T.pack $ show col <> [row]

instance FromJSON Tile where
  parseJSON = withText "Tile" $ \text1 -> do
    (col, text2) <- either fail pure $ Read.decimal text1
    (row, text3) <- maybe (fail "missing row") pure $ T.uncons text2

    unless (col >= 1 && col <= 12) $
      fail "column out-of-bounds"

    unless (row >= 'A' && row <= 'I') $
      fail "row out-of-bounds"

    unless (T.null text3) $
      fail $ "unexpected leftover input: " <> show text3

    return $ Tile col row

-- | The set of all possible tiles
allTiles :: [Tile]
allTiles = Tile <$> [1..12] <*> ['A'..'I']

-- | Which game zone is a tile currently in?
data TileLoc
  = Pool          -- ^ The drawing pool
  | Hand PlayerId -- ^ In a player's hand
  | Play          -- ^ On the game board
  | Discard       -- ^ Discarded and unusable
  deriving (Eq, Show)

-- | Companies in which players may invest
data Company
  = Triangle
  | Love
  | Armenian
  | Fiesta
  | Wonder
  | Century
  | Important
  deriving (Enum, Eq, Show, Ord, Generic)

instance ToJSON Company
instance FromJSON Company

-- | Each player and the bank hold a collection of stocks in each
-- company. This type represents a collection of stock assets.
type Stocks = Map Company Int

--------------------------------------------------------------------------------
-- Game State

-- | The complete set of information necessary to describe the game
-- state
data GameState = GameState
  { gameTiles   :: Map Tile TileLoc -- ^ The current state of each game tile
  , gameMarkers :: Map Company Tile -- ^ The current state of each company marker
  , gameMoney   :: Map PlayerId Money -- ^ The distribution of money
  , gameStocks  :: Map PlayerId Stocks -- ^ The distribution of stocks
  , gameRNG     :: StdGen -- ^ A source of random numbers for drawing
  } deriving (Eq, Show)

defaultGame :: GameState
defaultGame = GameState
  { gameTiles   = Map.fromList $ map (,Pool) allTiles
  , gameMarkers = Map.empty
  , gameMoney   = Map.singleton 0 242000 -- 60*100 + 40*500 + 36*1000 + 36*5000
  , gameStocks  = Map.singleton 0 defaultBankStocks
  , gameRNG     = mkStdGen 0
  }
  where
    defaultBankStocks = Map.fromList $ map (,25) [Triangle ..]

-- | Create a default new game with a random RNG seed.
newGameState :: MonadIO m => m GameState
newGameState = do
  gen <- getStdGen
  return defaultGame { gameRNG = gen }

-- | A monad for game-related actions
type Game = StateT GameState

-- | Events represents game actions which alter the game state and
-- must be broadcast to all players.
data Event
  = JoinEvent    PlayerId -- ^ A new player is joining
  | DrawEvent    PlayerId (Maybe Tile) -- ^ Draw a tile
  | PlayEvent    PlayerId Tile -- ^ Put a tile on the board
  | DiscardEvent PlayerId Tile -- ^ Discard an unusable tile
  | ReturnEvent  PlayerId Tile -- ^ Return a pile to the pool
  | MarkerEvent  PlayerId Company (Maybe Tile) -- ^ Place or remove a company marker tile
  | MoneyEvent   PlayerId Money -- ^ Take or return money
  | StockEvent   PlayerId Company Int -- ^ Take or return stocks
  deriving (Eq, Show)

-- | From which player did this event originate?
eventSource :: Event -> PlayerId
eventSource (JoinEvent    pid)     = pid
eventSource (DrawEvent    pid _)   = pid
eventSource (PlayEvent    pid _)   = pid
eventSource (DiscardEvent pid _)   = pid
eventSource (ReturnEvent  pid _)   = pid
eventSource (MarkerEvent  pid _ _) = pid
eventSource (MoneyEvent   pid _)   = pid
eventSource (StockEvent   pid _ _) = pid
