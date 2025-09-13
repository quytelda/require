{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Server where

import           Conduit
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Data.Bifunctor
import           Data.ByteString.Builder  as B
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
    (runTCPServer (serverSettings 11073 "*") (serveClient server))

-- | This thread handles the main game loop and runs for as long as
-- the server is alive.
runGameThread :: Server -> IO ()
runGameThread server = do
  game <- newGameState
  runConduit
    $ sourceTQueue server.recvQueue
    .| evalStateLC game gameConduit
    .| eitherC (iterMC printError .| errorSink) (iterMC printEvent .| eventSink)
  where
    errorSink = awaitForever $ \err -> liftIO $ atomically
      $ Map.lookup (errorSource err)
      <$> readTVar (server.clients)
      >>= \case Just queue -> writeTQueue queue (Left err)
                Nothing    -> throwM $ NoClientWithPid (errorSource err)
    eventSink = getZipSink
      $  ZipSink (broadcast server)
      *> ZipSink (sinkHistory server)

gameConduit
  :: MonadCatch m
  => ConduitT Event (Either RequireException Event) (Game m) ()
gameConduit = mapMC $ \event ->
  first (EventException event)
  <$> (try . handleEvent) event

-- | The life-cycle thread of a client.
serveClient :: Server -> AppData -> IO ()
serveClient server app = do
  -- A new client has just connected.
  pid <- newPlayerId server
  sendQueue <- newTQueueIO
  putBuilder
    $ "New connection from "
    <> show8 (appSockAddr app)
    <> ", PID: " <> B.intDec pid

  -- Wait for the client to initiate a handshake.
  runConduit
    $ appSource app
    .| handshake pid
    .| appSink app
  putBuilder
    $ "Handshake completed, PID: "
    <> B.intDec pid

  -- Register the connection in the client table. Past this point, we
  -- need to be concerned about cleaning up resources if the client
  -- exits.
  finally
    (do history <- atomically
          $  readTVar server.eventHistory
          <* modifyTVar' server.clients (Map.insert pid sendQueue)

        -- Stream incoming events to the global incoming queue while
        -- streaming outgoing events from the client's outgoing queue.
        race_
          (runConduit $ streamIncoming server pid app sendQueue)
          (runConduit $ streamOutgoing app sendQueue history)
    )
    (do putBuilder
          $ "Client disconnected, PID: "
          <> B.intDec pid
        atomically $ modifyTVar' server.clients $ Map.delete pid
    )

streamIncoming
  :: MonadIO m
  => Server
  -> PlayerId
  -> AppData
  -> MessageQueue
  -> ConduitT i o m ()
streamIncoming server pid app sendQueue =
  appSource app
  .| parseEvents pid
  .| eitherC handleErrors (sinkTQueue server.recvQueue)
  where
    handleErrors =
      mapC (ParseException pid)
      .| iterMC printError
      .| mapC Left
      .| sinkTQueue sendQueue

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
  -> ConduitT Event o m ()
broadcast server = awaitForever $ \event -> liftIO $ do
  clientMap <- readTVarIO server.clients
  forM_ clientMap $ atomically . flip writeTQueue (Right event)
