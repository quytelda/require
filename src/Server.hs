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
    errorSink = awaitForever $ \err -> liftIO $ do
      let pid = errorSource err
      clientMap <- readTVarIO (server.clients)
      case Map.lookup pid clientMap of
           Just queue -> atomically $ writeTQueue queue (Left err)
           Nothing    -> errBuilder $ "warning: PID " <> B.intDec pid <> " has no outgoing queue"
    eventSink = getZipSink
      $  ZipSink (broadcast server)
      *> ZipSink (sinkHistory server)

gameConduit
  :: MonadCatch m
  => ConduitT Event (Either RequireException Event) (Game m) ()
gameConduit = mapMC $ \event ->
  first (EventException event)
  <$> (try . handleEvent) event

createClient :: Server -> AppData -> Maybe PlayerId -> STM Client
createClient server app mpid = do
  maxUsedPid <- readTVar server.uidSource
  clientMap  <- readTVar server.clients
  pid <- case mpid of
    Nothing -> newPlayerId server
    Just requestedPid ->
      if requestedPid <= 0
         || requestedPid > maxUsedPid
         || Map.member requestedPid clientMap
      then throwM $ PidUnavailable requestedPid
      else return requestedPid
  queue <- newTQueue
  writeTVar server.clients (Map.insert pid queue clientMap)
  return $ Client app pid queue

-- | The life-cycle thread of a client.
serveClient :: Server -> AppData -> IO ()
serveClient server app = do
  let clientAddr = show8 (appSockAddr app)
  putBuilder $ "New connection from " <> clientAddr

  -- Wait for the client to iniate the handshake.
  mRequestedPid <- onException
    (runConduit $ appSource app .| readGreeting)
    (errBuilder $ "Invalid greeting from " <> clientAddr)

  -- Register the connection in the client table. Past this point, we
  -- need to be concerned about cleaning up resources if the client
  -- exits.
  (client, history) <- atomically $ (,)
    <$> createClient server app mRequestedPid
    <*> readTVar server.eventHistory

  let context = "[PID " <> B.intDec client.playerId <> "] "
  putBuilder
    $ context
    <> "established connection with "
    <> clientAddr

  finally
    (do runConduit $ sendGreeting client.playerId .| appSink app

        -- Stream incoming events to the global incoming queue while
        -- streaming outgoing events from the client's outgoing queue.
        race_
          (runConduit $ streamIncoming server client)
          (runConduit $ streamOutgoing client history)
    )
    (do putBuilder $ context <> "client disconnected"
        atomically $ modifyTVar' server.clients $ Map.delete client.playerId
    )

streamIncoming
  :: MonadIO m
  => Server
  -> Client
  -> ConduitT i o m ()
streamIncoming server client =
  appSource client.appData
  .| parseEvents client.playerId
  .| eitherC handleErrors (sinkTQueue server.recvQueue)
  where
    handleErrors =
      mapC (ParseException client.playerId)
      .| iterMC printError
      .| mapC Left
      .| sinkTQueue client.sendQueue

streamOutgoing
  :: MonadIO m
  => Client
  -> Seq Event
  -> ConduitT i o m ()
streamOutgoing client history =
  (yieldEvents history >> sourceTQueue client.sendQueue)
  .| eitherC renderErrors renderEvents
  .| appSink client.appData
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
