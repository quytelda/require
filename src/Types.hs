{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Types
  ( -- * Basic Game Types
    Money
  , PlayerId
  , Tile
  , renderTile
  , TileZone(..)
  , Company(..)

    -- * Game State
  , Stocks
  , GameState(..)
  , defaultGame
  , newGameState
  , Game(..)
  , throwGame
  , GameAction
  , runGameAction
  , withEvent

    -- * Events
  , Event(..)
  , eventSource
  , renderEventType
  , displayEvent
  , ServerEvent(..)

    -- * Errors
  , GameError(..)
  , gameErrorToServerError

    -- * Server Types
  , ServerId
  , ServerState(..)
  , newServerState
  , newPlayerId
    -- ** Event History
  , appendHistory
  , nthEvent
  , sourceHistory
  , logEvents

  -- * Utility Functions
  , textBuilderToJSON
  , putBuilderLn
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Aeson
import           Data.ByteString.Builder
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Sequence              (Seq, (|>))
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import           Data.Word
import           Network.Wai.Handler.Warp   (Port)
import           Servant.Types.SourceT      as Source
import           System.Random

import           Error
import           Event
import           Game.Internal

--------------------------------------------------------------------------------
-- Game State

-- | Each player and the bank hold a collection of stocks in each
-- company. This type represents a collection of stock assets.
type Stocks = Map Company Int

-- | The complete set of information necessary to describe the game
-- state
data GameState = GameState
  { gamePlayers :: IntMap Text -- ^ The set of active 'PlayerId's
  , gameTiles   :: Map Tile TileZone -- ^ The current state of each game tile
  , gameMarkers :: Map Company Tile -- ^ The current state of each company marker
  , gameMoney   :: Map PlayerId Money -- ^ The distribution of money
  , gameStocks  :: Map PlayerId Stocks -- ^ The distribution of stocks
  , gameRNG     :: StdGen -- ^ A source of random numbers for drawing
  } deriving (Eq, Show)

instance ToJSON GameState where
  toJSON GameState{..} = object
    [ "players" .= toJSON gamePlayers
    , "tiles" .= toJSON gameTiles
    , "markers" .= toJSON gameMarkers
    , "money" .= toJSON gameMoney
    , "stocks" .= toJSON gameStocks
    ]

defaultGame :: GameState
defaultGame = GameState
  { gamePlayers = IntMap.empty
  , gameTiles   = Map.fromList $ map (,Pool) allTiles
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

-- | A monad for game-related computations
newtype Game a = Game { runGame :: TVar GameState -> STM a }

instance Functor Game where
  fmap f (Game run) = Game $ fmap f . run

instance Applicative Game where
  pure = Game . const . pure
  (Game runF) <*> (Game runX) = Game $ liftA2 (<*>) runF runX

instance Monad Game where
  return = pure
  (Game runX) >>= f = Game $ \tvar -> do
    x <- runX tvar
    runGame (f x) tvar

instance MonadState GameState Game where
  get = Game readTVar
  put = Game . flip writeTVar

throwGame :: GameError -> Game a
throwGame = Game . const . throwSTM

-- | A game action that generates an event.
type GameAction a = Game (Event, a)

-- | Run an action and publish the event it generates.
runGameAction :: ServerState -> GameAction a -> STM a
runGameAction server action = do
  (event, result) <- runGame action (gameState server)
  appendHistory server event
  return result

-- | Utility function for writing 'GameActions'.
withEvent :: Event -> Game a -> GameAction a
withEvent = fmap . (,)

--------------------------------------------------------------------------------
-- Events

--------------------------------------------------------------------------------
-- Errors

--------------------------------------------------------------------------------
-- Server Types

type ServerId = Word32

data ServerState = ServerState
  { serverPort   :: Port -- ^ Which port this server is running on
  , serverId     :: ServerId -- ^ A random id to help clients recognize new sessions
  , pidSource    :: TVar PlayerId -- ^ A source for unique 'PlayerId's
  , eventHistory :: TVar (Seq Event) -- ^ History of successful game events
  , gameState    :: TVar GameState -- ^ The current state of the game
  }

newServerState :: IO ServerState
newServerState =
  ServerState 11073
  <$> randomIO
  <*> newTVarIO 0
  <*> newTVarIO mempty
  <*> (newGameState >>= newTVarIO)

-- | Generate a new 'PlayerId' guaranteed to be unique for this
-- 'Server'.
newPlayerId :: ServerState -> STM Int
newPlayerId ServerState{..} = modifyTVar' pidSource (+1) *> readTVar pidSource

-- | Add an event to the server's event history.
appendHistory :: ServerState -> Event -> STM ()
appendHistory ServerState{..} =
  modifyTVar' eventHistory . flip (|>)

-- | Get the 'n'th event in the history, or block if that event doesn't
-- exist yet. 'n' is an index must be a non-negative integer.
nthEvent :: ServerState -> Int -> IO Event
nthEvent ServerState{..} n = atomically $
  Seq.lookup n
  <$> readTVar eventHistory
  >>= maybe retry pure

-- | A finite stream of history values from 'start' to 'end'.
sourceHistory :: ServerState -> Int -> SourceT IO ServerEvent
sourceHistory server start = fromStepT $ Source.Yield comment (go start)
  where
    comment = CommentEvent $ "Server Events from " <> intDec start
    go n = Source.Effect $ do
      event <- nthEvent server n
      return $ Source.Yield (ServerEvent n event) (go (n+1))

-- | An event sent by a server.
data ServerEvent
  = ServerEvent Int Event
  | CommentEvent Builder
  deriving (Show)

logEvents :: ServerState -> IO ()
logEvents server =
  mapM_ (\n -> nthEvent server n >>= printEventWithIndex n) [0..]
  where
    paddedDecimal width =
      TB.fromLazyText
      . TL.justifyRight width '0'
      . TB.toLazyText
      . TBI.decimal
    printEventWithIndex n event = putBuilderLn $
      "[EVENT/" <> paddedDecimal 4 n <> "]: "
      <> displayEvent event
