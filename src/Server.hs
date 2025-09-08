{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Server where

import           Conduit
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.ByteString          (ByteString)
import           Data.Conduit.Network
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

import           Protocol
import           Types

sinkTQueue :: MonadIO m => TQueue a -> ConduitT a o m ()
sinkTQueue queue = awaitForever $ liftIO . atomically . writeTQueue queue

sourceTQueue :: MonadIO m => TQueue a -> ConduitT i a m ()
sourceTQueue queue =
  forever $ (liftIO . atomically . readTQueue) queue >>= yield

data Server = Server
  { uidSource :: TVar PlayerId
  , clients   :: TVar (Map PlayerId (TQueue Event))
  , recvQueue :: TQueue Event
  }

newServer :: IO Server
newServer =
  Server
  <$> newTVarIO 0
  <*> newTVarIO Map.empty
  <*> newTQueueIO

-- | Generate a new 'PlayerId' guaranteed to be unique for this
-- 'Server'.
newUID :: Server -> IO PlayerId
newUID server = atomically $ do
  modifyTVar' (uidSource server) (+1)
  u <- readTVar (uidSource server)
  return u

-- | Broadcast an event to all clients.
broadcast :: MonadIO m => Server -> ConduitT Event Void m ()
broadcast server = awaitForever $ \x -> yield x .| sendAll
  where
    sendAll = liftIO (readTVarIO server.clients)
              >>= sequenceSinks . fmap sinkTQueue

-- | Begin accepting connections from clients
runServer :: IO ()
runServer = do
  server <- newServer
  concurrently_
    (runConduit $ sourceTQueue server.recvQueue .| iterMC print .| broadcast server)
    (runTCPServer (serverSettings 11073 "127.0.0.1") $ \app -> do
        uid <- newUID server
        sendQueue <- newTQueueIO
        atomically $ modifyTVar' server.clients $ Map.insert uid sendQueue
        putStrLn
          $ "New connection from "
          <> show (appSockAddr app)
          <> maybe mempty (\s -> " (" <> show s <> ")") (appLocalAddr app)
          <> ", UID: "
          <> show uid

        race_
          (runConduit $ appSource app .| parseEvents .| sinkTQueue server.recvQueue)
          (runConduit $ sourceTQueue sendQueue .| renderEvents .| appSink app)

        putStrLn $ "Client disconnected, UID: " <> show uid
        atomically $ modifyTVar' server.clients $ Map.delete uid
    )

