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
  { serverClients   :: TVar (Map PlayerId Player)
  , serverSendQueue :: TQueue Event
  , serverRecvQueue :: TQueue Event
  }

newServer :: IO Server
newServer =
  Server
  <$> newTVarIO Map.empty
  <*> newTQueueIO
  <*> newTQueueIO

registerPlayer :: Server -> AppData -> STM Player
registerPlayer server appData = do
  clientMap <- readTVar (serverClients server)
  let newPlayerId = 1 + if null clientMap then 0 else maximum (Map.keys clientMap)
      newPlayer = Player newPlayerId appData
  writeTVar (serverClients server) (Map.insert newPlayerId newPlayer clientMap)
  return newPlayer

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
    putStrLn $ "Player #" <> show (playerId player) <>" connected"

    runConduitRes
      $ appSource appData
      .| parseEvents
      .| sinkRecvQueue server
