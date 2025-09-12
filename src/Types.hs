{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections             #-}

module Types
  ( -- * Basic Game Types
    Money
  , PlayerId
  , newPlayerId
  , Tile
  , TileLoc(..)
  , Company(..)
  , Stocks
  , GameState(..)
  , defaultGame
  , newGameState
  , Game

    -- * Events
  , Event(..)
  , eventSource

    -- * Exceptions
  , GameError(..)
  , RequireException(..)

    -- * Server Types
  , MessageQueue
  , Server(..)
  , newServer
  , sinkTQueue
  , sourceTQueue
  , sinkHistory

    -- * Useful Conduits
  , eitherC

    -- * Utility Functions
  , putBuilder
  , errBuilder
  , show8
  ) where

import           Conduit
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.State
import qualified Data.ByteString.Builder as B
import           Data.Conduit.Attoparsec
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Sequence           (Seq, (|>))
import           System.IO               (stderr, stdout)
import           System.Random

type Money = Int
type PlayerId = Int

-- | Tile coordinates (e.g. "9-F"): the column is an integer between 1
-- and 12 (inclusive) and the row is a character between 'A' and 'I'
-- (inclusive).
type Tile = (Int, Char)

-- | The set of all possible tiles
allTiles :: [Tile]
allTiles = (,) <$> [1..12] <*> ['A'..'I']

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
  deriving (Enum, Eq, Show, Ord)

-- | Each player and the bank hold a collection of stocks in each
-- company. This type represents a collection of stock assets.
type Stocks = Map Company Int

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
  = JoinEvent    PlayerId       -- ^ A new player is joining
  | DrawEvent    PlayerId (Maybe Tile) -- ^ Draw a tile
  | PlayEvent    PlayerId Tile -- ^ Put a tile on the board
  | DiscardEvent PlayerId Tile -- ^ Discard an unusable tile
  | ReturnEvent  PlayerId Tile -- ^ Return a pile to the pool
  | MarkerEvent  PlayerId Company (Maybe Tile) -- ^ Place or remove a company marker tile
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
-- Exceptions

data GameError
  = InvalidPlayerId PlayerId
  | NotEnoughTiles
  | NotEnoughMoney PlayerId
  | NotEnoughStock PlayerId Company
  | MissingTile PlayerId Tile
  deriving (Eq, Show)

formatPidSource :: PlayerId -> String
formatPidSource 0   = "the bank"
formatPidSource pid = "player #" <> show pid

instance Exception GameError where
  displayException (InvalidPlayerId pid) = "player ID is invalid: " <> show pid
  displayException NotEnoughTiles = "the draw pool is empty"
  displayException (NotEnoughMoney pid) = formatPidSource pid
                                          <> " doesn't have enough money"
  displayException (NotEnoughStock pid com) = formatPidSource pid
                                              <> " doesn't have enough "
                                              <> show com
                                              <> " stock"
  displayException (MissingTile pid tile) = formatPidSource pid
                                            <> " doesn't have control of tile "
                                            <> showTile tile
    where showTile (col, row) = show col <> "-" <> pure row

-- | Exceptions that can be safely handled by the server without
-- crashing any threads.
data RequireException
  = ParseException ParseError
  | EventException Event GameError
  deriving (Show)

instance Exception RequireException

--------------------------------------------------------------------------------
-- Server Types

type MessageQueue = TQueue (Either RequireException Event)

data Server = Server
  { uidSource    :: TVar PlayerId
  , clients      :: TVar (Map PlayerId MessageQueue)
  , recvQueue    :: TQueue Event
  , eventHistory :: TVar (Seq Event)
  }

newServer :: IO Server
newServer =
  Server
  <$> newTVarIO 0
  <*> newTVarIO mempty
  <*> newTQueueIO
  <*> newTVarIO mempty

-- | Generate a new 'PlayerId' guaranteed to be unique for this
-- 'Server'.
newPlayerId :: Server -> IO PlayerId
newPlayerId server = atomically $ do
  modifyTVar' (uidSource server) (+1)
  readTVar (uidSource server)

sinkTQueue :: MonadIO m => TQueue a -> ConduitT a o m ()
sinkTQueue queue = awaitForever $ liftIO . atomically . writeTQueue queue

sourceTQueue :: MonadIO m => TQueue a -> ConduitT i a m ()
sourceTQueue queue =
  forever $ (liftIO . atomically . readTQueue) queue >>= yield

-- | Add an event to the server's event history.
sinkHistory :: MonadIO m => Server -> ConduitT Event o m ()
sinkHistory server = awaitForever $
  liftIO . atomically . modifyTVar' (eventHistory server) . flip (|>)

--------------------------------------------------------------------------------
-- Misc. Useful Conduits

eitherC
  :: Monad m
  => ConduitT a c m ()
  -> ConduitT b c m ()
  -> ConduitT (Either a b) c m ()
eitherC left right =
  void $ getZipConduit $ (,)
  <$> ZipConduit (leftsC  .| left)
  <*> ZipConduit (rightsC .| right)

leftsC :: Monad m => ConduitT (Either a b) a m ()
leftsC = awaitForever $ either yield (const $ pure ())

rightsC :: Monad m => ConduitT (Either a b) b m ()
rightsC = awaitForever $ either (const $ pure ()) yield

--------------------------------------------------------------------------------
-- Utility Functions

putBuilder :: B.Builder -> IO ()
putBuilder = B.hPutBuilder stdout

errBuilder :: B.Builder -> IO ()
errBuilder = B.hPutBuilder stderr

show8 :: Show a => a -> B.Builder
show8 = B.string8 . show
