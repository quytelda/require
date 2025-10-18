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
  , TileZone(..)
  , Company(..)

    -- * Game State
  , Stocks
  , GameState(..)
  , defaultGame
  , newGameState
  , Game(..)
  , throwGame

    -- * Events
  , Event(..)
  , eventSource
  , displayEvent

    -- * Errors
  , GameError(..)
  , gameErrorToServerError
  , ServiceError(..)

    -- * Server Types
  , ServerId
  , ServerState(..)
  , newServerState
  , newPlayerId
  , appendHistory
  , sourceHistoryRange
  , sourceHistory

  -- * Utility Functions
  , textBuilderToJSON
  , putBuilderLn
  ) where

import           Control.Applicative
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
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Sequence              (Seq, (!?), (|>))
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import           Data.Text.Lazy.Encoding    (encodeUtf8Builder)
import qualified Data.Text.Lazy.IO          as TLIO
import qualified Data.Text.Read             as Read
import           Servant.Types.SourceT      as Source

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

renderTile :: Tile -> TB.Builder
renderTile (Tile col row) = TBI.decimal col <> TB.singleton row

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
  toJSON = textBuilderToJSON . renderTile

instance FromJSON Tile where
  parseJSON = withText "Tile" $ either fail pure . parseTile

instance ToJSONKey Tile where
  toJSONKey = toJSONKeyText (TL.toStrict . TB.toLazyText . renderTile)

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

renderTileZone :: TileZone -> TB.Builder
renderTileZone Pool       = "pool"
renderTileZone (Hand pid) = "hand/" <> TBI.decimal pid
renderTileZone Play       = "play"
renderTileZone Discard    = "discard"

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
  toJSON = textBuilderToJSON . renderTileZone

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

renderCompany :: Company -> TB.Builder
renderCompany Triangle  = "Triangle"
renderCompany Love      = "Love"
renderCompany Armenian  = "Armenian"
renderCompany Fiesta    = "Fiesta"
renderCompany Wonder    = "Wonder"
renderCompany Century   = "Century"
renderCompany Important = "Important"

instance ToJSON Company
instance FromJSON Company
instance ToJSONKey Company

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

instance ToJSON GameState where
  toJSON GameState{..} = object
    [ "tiles" .= toJSON gameTiles
    , "markers" .= toJSON gameMarkers
    , "money" .= toJSON gameMoney
    , "stocks" .= toJSON gameStocks
    ]

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

displayEvent :: Event -> TB.Builder
displayEvent event =
  "[EVENT/" <> TBI.decimal (eventSource event) <> "]: "
  <> displayEvent' event
  where
    displayEvent' (JoinEvent   _) = "JOIN"
    displayEvent' (DrawEvent   _) = "DRAW"
    displayEvent' (MoveEvent   _ tile src dst) =
      "MOVE " <> renderTile tile
      <> " from " <> renderTileZone src
      <> " to " <> renderTileZone dst
    displayEvent' (MarkerEvent _ com mtile) =
      "MARKER for " <> renderCompany com
      <> case mtile of
           Just tile -> " to " <> renderTile tile
           Nothing   -> " removed"
    displayEvent' (MoneyEvent  _ amount) =
      "MONEY transfer of " <> TBI.decimal amount
    displayEvent' (StockEvent  _ com amount) =
      "STOCK transfer of " <> TBI.decimal amount
      <> " " <> renderCompany com

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
      <> (tl2bs . renderCompany) com
      <> " stock"
    describeGameError' (InvalidMove tile src dst) =
      "cannot move tile "
      <> (tl2bs . renderTile) tile
      <> " from "
      <> (tl2bs . renderTileZone) src
      <> " to "
      <> (tl2bs . renderTileZone) dst

    renderPid 0   = "the bank"
    renderPid pid = "player #" <> intDec pid

    tl2bs = encodeUtf8Builder . TB.toLazyText

gameErrorToServerError :: GameError -> ServerError
gameErrorToServerError err = err400 { errBody = describeGameError err }

data ServiceError
  = UnknownPid PlayerId
  deriving (Eq, Show)

instance Exception ServiceError

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

-- | Get the 'n'th event in the history, or block if that event doesn't
-- exist yet. 'n' is an index must be a non-negative integer.
nthEvent :: ServerState -> Int -> IO Event
nthEvent ServerState{..} n = atomically $
  Seq.lookup n
  <$> readTVar eventHistory
  >>= maybe retry pure

-- | A finite stream of history values from 'start' to 'end'.
sourceHistoryRange :: ServerState -> Int -> Int -> SourceT IO Event
sourceHistoryRange server start end = fromStepT (go start)
  where
    go n | n > end = Source.Stop
         | otherwise = Source.Effect $ do
             event <- nthEvent server n
             return $ Source.Yield event (go (n+1))

-- | Get the next unread event in the history for the given player.
--
-- If the 'PlayerId' requested is not registered, 'Nothing' is
-- returned. If no new events are available, we wait.
popHistory :: ServerState -> PlayerId -> STM (Maybe Event)
popHistory ServerState{..} pid = do
  history <- readTVar eventHistory
  positions <- readTVar clientOffsets
  case Map.lookup pid positions of
    Nothing -> return Nothing
    Just offset ->
      case history !? offset of
        Nothing -> retry
        Just event -> do
          modifyTVar' clientOffsets $ Map.adjust (+1) pid
          return $ Just event

-- | A SourceT that streams events from the event history one a time
-- as they arrive.
sourceHistory :: ServerState -> PlayerId -> SourceT IO Event
sourceHistory server pid = fromStepT loop
  where
    loop = Source.Effect $ do
      mevent <- atomically $ popHistory server pid
      return $ case mevent of
        Nothing    -> Source.Stop
        Just event -> Source.Yield event loop

--------------------------------------------------------------------------------
-- Utility Functions

textBuilderToJSON :: TB.Builder -> Value
textBuilderToJSON = String . TL.toStrict . TB.toLazyText

putBuilderLn :: TB.Builder -> IO ()
putBuilderLn = TLIO.putStrLn . TB.toLazyText
