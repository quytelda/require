{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Server where

import           Conduit
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Conduit.Network
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

import           Game
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

serveClient :: Server -> AppData -> IO ()
serveClient server app = do
  -- A new client has just connected.
  uid <- newUID server
  sendQueue <- newTQueueIO
  putStrLn
    $ "New connection from "
    <> show (appSockAddr app)
    <> ", UID: " <> show uid

  -- Wait for the client to initiate a handshake.
  runConduit
    $ appSource app
    .| handshake uid
    .| appSink app

  -- Register the connection in the client table.
  atomically $ modifyTVar' server.clients $ Map.insert uid sendQueue

  -- Stream incoming events to the global incoming queue while
  -- streaming outgoing events from the client's outgoing queue.
  race_
    (runConduit $ appSource app .| parseEvents .| sinkTQueue server.recvQueue)
    (runConduit $ sourceTQueue sendQueue .| renderEvents .| appSink app)

  -- The client has now disconnected, so we can clean up.
  putStrLn $ "Client disconnected, UID: " <> show uid
  atomically $ modifyTVar' server.clients $ Map.delete uid

-- | Begin accepting connections from clients
runServer :: IO ()
runServer = do
  server <- newServer
  concurrently_
    (runConduit $ sourceTQueue server.recvQueue .| gameConduit .| broadcast server)
    (runTCPServer (serverSettings 11073 "127.0.0.1") (serveClient server))

gameConduit :: (MonadIO m, MonadCatch m) => ConduitT Event Event m ()
gameConduit = evalStateLC defaultGame $ awaitForever $ \event -> do
  result <- lift $ try $ handleEvent event
  case result of
    Right event' -> yield event' >> lift get >>= liftIO . print
    Left  err    -> handleError err
  where
    handleError err = liftIO $ print (err :: GameError)
