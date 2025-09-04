{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server where

import           Conduit
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Conduit.Network
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Protocol
import           Types

-- | Represents a connection with a player's client.
data Player = Player
  { playerId  :: PlayerId
  , playerApp :: AppData
  }

playerSource
  :: (MonadIO m, MonadThrow m)
  => Player
  -> ConduitT i Event m ()
playerSource Player{..} = appSource playerApp .| parseEvents

playerSink
  :: (MonadIO m, PrimMonad m)
  => Player
  -> ConduitT Event o m ()
playerSink Player{..} = renderEvents .| appSink playerApp

data Server = Server
  { serverPlayers   :: TVar (Map PlayerId Player)
  , serverSendQueue :: TQueue Event
  , serverRecvQueue :: TQueue Event
  }

newServer :: IO Server
newServer =
  Server
  <$> newTVarIO Map.empty
  <*> newTQueueIO
  <*> newTQueueIO

newPlayerId :: Server -> STM PlayerId
newPlayerId server = do
  players <- readTVar (serverPlayers server)
  return $
    if null players
    then 1
    else 1 + maximum (Map.keys players)

registerPlayer :: Server -> AppData -> STM Player
registerPlayer server appData = do
  pid <- newPlayerId server
  let player = Player pid appData
  modifyTVar' (serverPlayers server) $ Map.insert pid player
  return player

unregisterPlayer :: Server -> Player -> STM ()
unregisterPlayer server = modifyTVar' (serverPlayers server) . Map.delete . playerId

-- | Begin accepting connections from clients
runServer :: IO ()
runServer = do
  server <- newServer
  runTCPServer (serverSettings 11073 "*") $ \appData -> do

    -- Registration
    player@Player{..} <- atomically $ registerPlayer server appData
    putStrLn $ "Player #" <> show playerId <> " connected"
    runConduitRes
      $ yield (HelloEvent playerId)
      .| playerSink player

    -- Stream incoming events to the global incoming queue.
    runConduitRes
      $ playerSource player
      .| sinkTQueue (serverRecvQueue server)

    -- TODO: Gracefully exit on connection errors and other issues.
    atomically $ unregisterPlayer server player
    putStrLn $ "Player #" <> show playerId <> " finished"

sinkTQueue :: MonadIO m => TQueue a -> ConduitT a o m ()
sinkTQueue queue = awaitForever $ liftIO . atomically . writeTQueue queue

sourceTQueue :: MonadIO m => TQueue a -> ConduitT i a m ()
sourceTQueue queue =
  forever $ (liftIO . atomically . readTQueue) queue >>= yield
