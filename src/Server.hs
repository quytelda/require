{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server where

import           Conduit
import           Control.Concurrent.STM
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

sinkSendQueue :: MonadResource m => Server -> ConduitT Event o m ()
sinkSendQueue server = awaitForever $ \e ->
  liftIO $ atomically $ writeTQueue (serverSendQueue server) e

sinkRecvQueue :: MonadResource m => Server -> ConduitT Event o m ()
sinkRecvQueue server = awaitForever $ \e ->
  liftIO $ atomically $ writeTQueue (serverRecvQueue server) e

-- | Begin accepting connections from clients
runServer :: IO ()
runServer = do
  server <- newServer
  runTCPServer (serverSettings 11073 "*") $ \appData -> do
    player <- atomically $ registerPlayer server appData
    putStrLn $ "Player #" <> show (playerId player) <> " connected"

    runConduitRes
      $ appSource appData
      .| parseEvents
      .| sinkRecvQueue server

    -- TODO: Gracefully exit on connection errors and other issues.
    atomically $ unregisterPlayer server player
    putStrLn $ "Player #" <> show (playerId player) <> " finished"
