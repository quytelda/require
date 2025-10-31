{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server.Types
  ( ServerId
  , ServerState(..)
  , newServerState
  , newPlayerId

    -- * Event History
  , ServerEvent(..)
  , appendHistory
  , nthEvent
  , sourceHistory
  , logEvents
  ) where

import           Control.Concurrent.STM
import           Data.ByteString.Builder
import           Data.Sequence              (Seq, (|>))
import qualified Data.Sequence              as Seq
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import           Data.Word
import           Network.Wai.Handler.Warp   (Port)
import           Servant.Types.SourceT      as Source
import           System.Random

import           Event
import           Game.Internal
import           Game.Types

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
