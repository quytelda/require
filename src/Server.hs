{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server where

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

-- | Begin accepting connections from clients
runServer :: IO ()
runServer = do
  server <- newServer
  runTCPServer (serverSettings 11073 "*") $ \appData -> do
    return ()
