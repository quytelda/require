{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Game.Types
  ( Stocks
  , GameState(..)
  , defaultGame
  , newGameState
  , Game(..)
  , throwGame
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Aeson
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import           System.Random

import           Error
import           Game.Internal

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
