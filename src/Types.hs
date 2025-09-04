{-# LANGUAGE RecordWildCards #-}
module Types where

import           Conduit
import           Control.Exception
import           Control.Monad
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

type Stocks = Map Company Int

-- | Game State
data Game = Game
  { gameTiles  :: Map Coord TileLoc
  , gameMoney  :: Map PlayerId Money
  , gameStocks :: Map PlayerId Stocks
  } deriving (Eq, Show)

defaultGame :: Game
defaultGame = Game
  { gameTiles = Map.empty
  , gameMoney = Map.singleton 0 242000 -- 60*100 + 40*500 + 36*1000 + 36*5000
  , gameStocks = Map.singleton 0 defaultBankStocks
  }
  where
    defaultBankStocks = Map.fromList $ zip [Triangle ..] (repeat 25)

addPlayer :: PlayerId -> Game -> Game
addPlayer pid game = game
  { gameMoney = Map.insert pid 0 $ gameMoney game
  , gameStocks = Map.insert pid Map.empty $ gameStocks game
  }

getMoney :: MonadThrow m => PlayerId -> Game -> m Money
getMoney pid Game{..} =
  maybe (throwM $ BadPlayerId pid) return $ Map.lookup pid gameMoney

setMoney :: MonadThrow m => PlayerId -> Money -> Game -> m Game
setMoney pid amount game
  | pid `Map.member` gameMoney game =
      return game { gameMoney = Map.insert pid amount (gameMoney game) }
  | otherwise = throwM $ BadPlayerId pid

getStocks :: MonadThrow m => PlayerId -> Company -> Game -> m Int
getStocks pid com =
  maybe (throwM $ BadPlayerId pid) (return . Map.findWithDefault 0 com)
  . Map.lookup pid
  . gameStocks

setStocks :: MonadThrow m => PlayerId -> Company -> Int -> Game -> m Game
setStocks pid com qty game = do
  stocks <- maybe (throwM $ BadPlayerId pid) return $ Map.lookup pid $ gameStocks game
  let stocks' = Map.insert com qty stocks
  return game
    { gameStocks = Map.insert pid stocks' (gameStocks game)
    }

transferMoney :: MonadThrow m => PlayerId -> Money -> Game -> m Game
transferMoney pid amount game = do
  bankBal   <- getMoney 0   game
  playerBal <- getMoney pid game

  unless (bankBal > amount) $
    throwM $ OutOfMoney 0

  unless (playerBal > negate amount) $
    throwM $ OutOfMoney pid

  setMoney 0 (bankBal - amount) game
    >>= setMoney pid (playerBal + amount)

transferStock :: MonadThrow m => PlayerId -> Company -> Int -> Game -> m Game
transferStock pid com amount game = do
  bankQty   <- getStocks 0   com game
  playerQty <- getStocks pid com game

  unless (bankQty > amount) $
    throwM $ OutOfStock 0 com

  unless (playerQty > negate amount) $
    throwM $ OutOfStock pid com

  setStocks 0 com (bankQty - amount) game
    >>= setStocks pid com (playerQty + amount)

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
