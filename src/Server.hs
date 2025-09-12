{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Server
  ( runServer
  ) where

import           Conduit
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Conduit.Network
import qualified Data.Map.Strict          as Map
import           Data.Sequence            (Seq)

import           Game
import           Protocol
import           Types

runServer :: IO ()
runServer = do
  server <- newServer
  concurrently_
    (runGameThread server)
    (runTCPServer (serverSettings 11073 "127.0.0.1") (serveClient server))

-- | This thread handles the main game loop and runs for as long as
-- the server is alive.
runGameThread :: Server -> IO ()
runGameThread server = runConduit
  $ sourceTQueue server.recvQueue
  .| gameLoop server
  .| sink
  where
    sink = getZipSink
      $  ZipSink (broadcast server)
      *> ZipSink (sinkHistory server)

gameLoop
  :: (MonadIO m, MonadCatch m)
  => Server
  -> ConduitT Event Event m ()
gameLoop server = do
  game <- newGameState
  evalStateLC game $ awaitForever $ \event -> do
    result <- lift $ try $ handleEvent event
    case result of
      Right event' -> do yield event'
                         lift get >>= liftIO . print
      Left  err    -> liftIO $ sendError event err
  where
    sendError :: Event -> GameError -> IO ()
    sendError event err = do
      let pid = eventSource event
      mqueue <- Map.lookup pid <$> readTVarIO server.clients
      case mqueue of
        Just queue -> atomically
          $ writeTQueue queue
          $ Left
          $ EventException event err
        Nothing -> putStrLn
          $ "Warning: unable to deliver error to player #" <> show pid

-- | The life-cycle thread of a client.
serveClient :: Server -> AppData -> IO ()
serveClient server app = do
  -- A new client has just connected.
  pid <- newPlayerId server
  sendQueue <- newTQueueIO
  putStrLn
    $ "New connection from "
    <> show (appSockAddr app)
    <> ", PID: " <> show pid

  -- Wait for the client to initiate a handshake.
  runConduit
    $ appSource app
    .| handshake pid
    .| appSink app

  -- Register the connection in the client table. Past this point, we
  -- need to be concerned about cleaning up resources if the client
  -- exits.
  history <- atomically
    $  readTVar server.eventHistory
    <* modifyTVar' server.clients (Map.insert pid sendQueue)

  -- Stream incoming events to the global incoming queue while
  -- streaming outgoing events from the client's outgoing queue.
  race_
    (runConduit $ streamIncoming server app sendQueue)
    (runConduit $ streamOutgoing app sendQueue history)

  -- The client has now disconnected, so we can clean up.
  putStrLn $ "Client disconnected, UID: " <> show pid
  atomically $ modifyTVar' server.clients $ Map.delete pid

streamIncoming
  :: MonadIO m
  => Server
  -> AppData
  -> MessageQueue
  -> ConduitT i o m ()
streamIncoming server app sendQueue =
  appSource app
  .| parseEvents
  .| eitherC handleErrors (sinkTQueue server.recvQueue)
  where
    handleErrors = mapC (Left . ParseException) .| sinkTQueue sendQueue

streamOutgoing
  :: MonadIO m
  => AppData
  -> MessageQueue
  -> Seq Event
  -> ConduitT i o m ()
streamOutgoing app sendQueue history =
  (yieldEvents history >> sourceTQueue sendQueue)
  .| eitherC renderErrors renderEvents
  .| appSink app
  where
    yieldEvents = yieldMany . fmap Right

-- | Broadcast an 'Event' to all clients connected to this server.
broadcast
  :: MonadIO m
  => Server
  -> ConduitT Event Void m ()
broadcast server = awaitForever $ \x -> yield (Right x) .| sendAll
  where
    sendAll = liftIO (readTVarIO server.clients)
              >>= sequenceSinks . fmap sinkTQueue
