{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Types
  ( -- * Basic Game Types
    Money
  , PlayerId
  , Tile
  , TileZone(..)
  , Company(..)

    -- * Game State
  , Stocks
  , GameState(..)
  , defaultGame
  , newGameState
  , Game
  , runGame
  , runGameSTM

    -- * Events
  , Event(..)
  , eventSource
  , GameError(..)
  , gameErrorToServerError

    -- * Server Types
  , ServerId
  , ServerState(..)
  , newServerState
  , newPlayerId
  , appendHistory

  -- * Utility Functions
  , textBuilderToJSON
  , putBuilderLn
  ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.ByteString.Builder
import           Data.ByteString.Lazy       (LazyByteString)
import           Data.Functor
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Sequence              (Seq, (|>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Lazy.IO          as TLIO
import qualified Data.Text.Read             as Read

import           Data.Word
import           GHC.Generics
import           Servant
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

parseTile :: Text -> Either String Tile
parseTile text1 = do
  (col, text2) <- Read.decimal text1
  (row, text3) <- maybe (Left "missing row") Right $ T.uncons text2

  unless (col >= 1 && col <= 12) $
    Left "column out-of-bounds"

  unless (row >= 'A' && row <= 'I') $
    Left "row out-of-bounds"

  unless (T.null text3) $
    Left $ "unexpected leftover input: " <> show text3

  return $ Tile col row

instance ToJSON Tile where
  toJSON (Tile col row) =
    textBuilderToJSON
    $ TBI.decimal col
    <> TB.singleton row

instance FromJSON Tile where
  parseJSON = withText "Tile" $ either fail pure . parseTile

instance FromHttpApiData Tile where
  parseQueryParam = first T.pack . parseTile

-- | The set of all possible tiles
allTiles :: [Tile]
allTiles = Tile <$> [1..12] <*> ['A'..'I']

-- | Which game zone is a tile currently in?
data TileZone
  = Pool          -- ^ The drawing pool
  | Hand PlayerId -- ^ In a player's hand
  | Play          -- ^ On the game board
  | Discard       -- ^ Discarded and unusable
  deriving (Eq, Show)

parseTileZone :: Text -> Either String TileZone
parseTileZone "pool"    = pure Pool
parseTileZone "play"    = pure Play
parseTileZone "discard" = pure Discard
parseTileZone txt =
  fmap (Hand . fst)
  . maybe (Left "invalid tile zone") Read.decimal
  . T.stripPrefix "hand/"
  $ txt

instance ToJSON TileZone where
  toJSON Pool       = String "pool"
  toJSON (Hand pid) = textBuilderToJSON $ "hand/" <> TBI.decimal pid
  toJSON Play       = String "play"
  toJSON Discard    = String "discard"

instance FromJSON TileZone where
  parseJSON = withText "TileZone" $ either fail pure . parseTileZone

instance FromHttpApiData TileZone where
  parseQueryParam = first T.pack . parseTileZone

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

instance FromHttpApiData Company where
  parseQueryParam "Triangle"  = Right Triangle
  parseQueryParam "Love"      = Right Love
  parseQueryParam "Armenian"  = Right Armenian
  parseQueryParam "Fiesta"    = Right Fiesta
  parseQueryParam "Wonder"    = Right Wonder
  parseQueryParam "Century"   = Right Century
  parseQueryParam "Important" = Right Important
  parseQueryParam _           = Left "invalid company name"

-- | Each player and the bank hold a collection of stocks in each
-- company. This type represents a collection of stock assets.
type Stocks = Map Company Int

--------------------------------------------------------------------------------
-- Game State

-- | The complete set of information necessary to describe the game
-- state
data GameState = GameState
  { gameTiles   :: Map Tile TileZone -- ^ The current state of each game tile
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
type Game = StateT GameState (Except GameError)

runGame :: Game a -> GameState -> Either GameError (a, GameState)
runGame g = runExcept . runStateT g

execGame :: Game a -> GameState -> Either GameError GameState
execGame g = runExcept . execStateT g

evalGame :: Game a -> GameState -> Either GameError a
evalGame g = runExcept . evalStateT g

runGameSTM :: Game a -> TVar GameState -> STM (Either GameError a)
runGameSTM g tv = do
  s <- readTVar tv
  case runGame g s of
    Left err      -> return (Left err)
    Right (a, s') -> writeTVar tv s' $> Right a

--------------------------------------------------------------------------------
-- Events

-- | Events represents game actions which alter the game state and
-- must be broadcast to all players.
data Event
  = JoinEvent   PlayerId -- ^ A new player is joining
  | DrawEvent   PlayerId -- ^ Draw a tile
  | MoveEvent   PlayerId Tile TileZone TileZone -- ^ Move a tile between zones
  | MarkerEvent PlayerId Company (Maybe Tile) -- ^ Place or remove a company marker tile
  | MoneyEvent  PlayerId Money -- ^ Take or return money
  | StockEvent  PlayerId Company Int -- ^ Take or return stocks
  deriving (Eq, Show)

-- | From which player did this event originate?
eventSource :: Event -> PlayerId
eventSource (JoinEvent   pid)       = pid
eventSource (DrawEvent   pid)       = pid
eventSource (MoveEvent   pid _ _ _) = pid
eventSource (MarkerEvent pid _ _)   = pid
eventSource (MoneyEvent  pid _)     = pid
eventSource (StockEvent  pid _ _)   = pid

instance ToJSON Event where
  toJSON (JoinEvent pid) =
    object [ "type" .= String "join"
           , "source" .= pid
           ]
  toJSON (DrawEvent pid) =
    object [ "type" .= String "draw"
           , "source" .= pid
           ]
  toJSON (MoveEvent pid tile srcZone dstZone) =
    object [ "type" .= String "move"
           , "source" .= pid
           , "tile" .= tile
           , "src" .= srcZone
           , "dst" .= dstZone
           ]
  toJSON (MarkerEvent pid com mtile) =
    object [ "type" .= String "marker"
           , "source" .= pid
           , "company" .= com
           , "tile" .= mtile
           ]
  toJSON (MoneyEvent pid amount) =
    object [ "type" .= String "money"
           , "source" .= pid
           , "amount" .= amount
           ]
  toJSON (StockEvent pid com amount) =
    object [ "type" .= String "stock"
           , "source" .= pid
           , "company" .= com
           , "amount" .= amount
           ]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \obj -> do
    evType <- obj .: "type" :: Parser Text
    pid <- obj .: "source" :: Parser PlayerId
    case evType of
      "join"    -> return $ JoinEvent pid
      "draw"    -> return $ DrawEvent pid
      "move"    -> MoveEvent pid
                   <$> obj .: "tile"
                   <*> obj .: "src"
                   <*> obj .: "dst"
      "marker"  -> MarkerEvent pid
                   <$> obj .: "company"
                   <*> obj .:? "tile"
      "money"   -> MoneyEvent pid
                   <$> obj .: "amount"
      "stock"   -> StockEvent pid
                   <$> obj .: "company"
                   <*> obj .: "amount"
      _         -> fail "invalid event type"

--------------------------------------------------------------------------------
-- Errors

-- | Game-related Errors
data GameError
  = NotEnoughTiles -- ^ The bank doesn't have enough tiles
  | NotEnoughMoney PlayerId -- ^ Insufficient funds for a transaction
  | NotEnoughStock PlayerId Company -- ^ Insufficient stock for a transaction
  | InvalidMove Tile TileZone TileZone -- ^ This tile movement is invalid
  deriving (Eq, Show)

instance Exception GameError

describeGameError :: GameError -> LazyByteString
describeGameError = toLazyByteString . describeGameError'
  where
    describeGameError' NotEnoughTiles =
      "not enough tiles in the drawing pool"
    describeGameError' (NotEnoughMoney pid) =
      renderPid pid
      <> " doesn't have enough money"
    describeGameError' (NotEnoughStock pid com) =
      renderPid pid
      <> " doesn't have enough "
      <> renderCompany com
      <> " stock"
    describeGameError' (InvalidMove tile fromZone toZone) =
      "cannot move tile "
      <> renderTile tile
      <> " from "
      <> renderTileZone fromZone
      <> " to "
      <> renderTileZone toZone

gameErrorToServerError :: GameError -> ServerError
gameErrorToServerError err = err400 { errBody = describeGameError err }

renderPid :: PlayerId -> Builder
renderPid 0   = "the bank"
renderPid pid = "player #" <> intDec pid

renderCompany :: Company -> Builder
renderCompany = string8 . show

renderTile :: Tile -> Builder
renderTile (Tile col row) = intDec col <> char8 row

renderTileZone :: TileZone -> Builder
renderTileZone Pool       = "the pool"
renderTileZone (Hand pid) = renderPid pid <> "'s hand"
renderTileZone Play       = "the board"
renderTileZone Discard    = "the discard pile"

--------------------------------------------------------------------------------
-- Server Types

type ServerId = Word32

data ServerState = ServerState
  { serverId      :: ServerId -- ^ A random id to help clients recognize new sessions
  , pidSource     :: TVar PlayerId -- ^ A source for unique 'PlayerId's
  , clientOffsets :: TVar (Map PlayerId Int) -- ^ How many messages has each client seen?
  , eventHistory  :: TVar (Seq Event) -- ^ History of successful game events
  , gameState     :: TVar GameState -- ^ The current state of the game
  }

newServerState :: IO ServerState
newServerState =
  ServerState
  <$> randomIO
  <*> newTVarIO 0
  <*> newTVarIO mempty
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

--------------------------------------------------------------------------------
-- Utility Functions

textBuilderToJSON :: TB.Builder -> Value
textBuilderToJSON = String . TL.toStrict . TB.toLazyText

putBuilderLn :: TB.Builder -> IO ()
putBuilderLn = TLIO.putStrLn . TB.toLazyText
