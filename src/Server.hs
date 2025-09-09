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
import           Data.Sequence            (Seq, (|>))
import qualified Data.Sequence            as Seq

import           Game
import           Protocol
import           Types

sinkTQueue :: MonadIO m => TQueue a -> ConduitT a o m ()
sinkTQueue queue = awaitForever $ liftIO . atomically . writeTQueue queue

sourceTQueue :: MonadIO m => TQueue a -> ConduitT i a m ()
sourceTQueue queue =
  forever $ (liftIO . atomically . readTQueue) queue >>= yield

-- | Network Server State
data Server = Server
  { uidSource    :: TVar PlayerId
  , clients      :: TVar (Map PlayerId (TQueue Event))
  , recvQueue    :: TQueue Event
  , eventHistory :: TVar (Seq Event)
  }

newServer :: IO Server
newServer =
  Server
  <$> newTVarIO 0
  <*> newTVarIO Map.empty
  <*> newTQueueIO
  <*> newTVarIO Seq.empty

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

-- | The life-cycle thread of a client.
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

  -- Register the connection in the client table. Past this point, we
  -- need to be concerned about cleaning up resources if the client
  -- exits.
  history <- atomically
    $ readTVar server.eventHistory
    <* modifyTVar' server.clients (Map.insert uid sendQueue)

  -- Stream incoming events to the global incoming queue while
  -- streaming outgoing events from the client's outgoing queue.
  race_
    (runConduit $ appSource app .| parseEvents .| sinkTQueue server.recvQueue)
    (runConduit $ (yieldMany history >> sourceTQueue sendQueue) .| renderEvents .| appSink app)

  -- The client has now disconnected, so we can clean up.
  putStrLn $ "Client disconnected, UID: " <> show uid
  atomically $ modifyTVar' server.clients $ Map.delete uid

-- | The server launches two primary threads: one runs the primary
-- game loop, while the other accepts incoming connections from new
-- clients.
runServer :: IO ()
runServer = do
  server <- newServer
  let sinkEvents = sequenceSinks [broadcast server, sinkHistory server]
  concurrently_
    (runConduit $ sourceTQueue server.recvQueue .| gameConduit .| sinkEvents)
    (runTCPServer (serverSettings 11073 "127.0.0.1") (serveClient server))

-- | Add an event to the server's event history.
sinkHistory :: MonadIO m => Server -> ConduitT Event o m ()
sinkHistory server = awaitForever $ \event ->
  liftIO $ atomically $ modifyTVar' server.eventHistory (|> event)

-- | Apply events to the game state. If the event causes an error,
-- drop the event.
gameConduit :: (MonadIO m, MonadCatch m) => ConduitT Event Event m ()
gameConduit = evalStateLC defaultGame $ awaitForever $ \event -> do
  result <- lift $ try $ handleEvent event
  case result of
    Right event' -> yield event' >> lift get >>= liftIO . print
    Left  err    -> handleError err
  where
    handleError err = liftIO $ print (err :: GameError)
