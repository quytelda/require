{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Data.Functor
import qualified Data.Map.Strict          as Map
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Network.Wai.Handler.Warp
import           Servant

import           Game
import           Types

type RequiredParam = QueryParam' '[Required, Strict]
type TileParam = RequiredParam "tile" Tile
type CompanyParam = RequiredParam "company" Company
type AmountParam = RequiredParam "amount" Int
type EventReq = Post '[JSON] NoContent

type RequireAPI
  =    "serverid" :> Get '[JSON] ServerId
  :<|> "register" :> Get '[JSON] PlayerId
  :<|> Capture "PlayerId" PlayerId
  :> (    "events" :> Get '[JSON] (Seq Event)
     :<|> "reset"  :> Get '[JSON] NoContent
     -- state queries
     :<|> "hand"   :> Get '[JSON] [Tile]
     :<|> "board"  :> Get '[JSON] [Tile]
     )
  :<|> "marker" :> Capture "Company" Company :> Get '[JSON] (Maybe Tile)
  :<|> Capture "PlayerId" PlayerId
  :> ("draw"
       :> Post '[JSON] Tile
     :<|> "move"
       :> TileParam
       :> RequiredParam "src" TileZone
       :> RequiredParam "dst" TileZone
       :> EventReq
     :<|> "marker"
       :> CompanyParam
       :> QueryParam "tile" Tile
       :> EventReq
     :<|> "money"
       :> AmountParam
       :> EventReq
     :<|> "stock"
       :> CompanyParam
       :> AmountParam
       :> EventReq
     )

requireServer :: ServerState -> Server RequireAPI
requireServer s =
  handleServerId s
  :<|> handleRegister s
  :<|> (\pid -> handleEvents s pid
         :<|> handleReset s pid
         :<|> handleHand s pid
         :<|> handleBoard s pid
       )
  :<|> handleQueryMarker s
  :<|> (\pid -> handleDraw s pid
         :<|> handleMove s pid
         :<|> handleMarker s pid
         :<|> handleMoney s pid
         :<|> handleStock s pid
       )

requireAPI :: Proxy RequireAPI
requireAPI = Proxy

runServer :: ServerState -> IO ()
runServer = run 11073 . serve requireAPI . requireServer

publish :: ServerState -> Event -> STM ()
publish = appendHistory

-- | An endpoint to retrieve the server's ID.
--
-- Clients should store this ID alongside their own player ID so that
-- when a new server is created, they will be able to tell that the
-- player ID has expired because the server ID no longer matches.
handleServerId :: ServerState -> Handler ServerId
handleServerId ServerState{..} = return serverId

-- | Handle the new client registration endpoint.
--
-- Generates a new player ID which is unique for this server and
-- allocates resources for managing this client.
handleRegister :: ServerState -> Handler PlayerId
handleRegister server = liftIO $ do
  pid <- atomically $ do
    pid <- newPlayerId server
    modifyTVar' (clientOffsets server) (Map.insert pid 0)
    return pid
  putStrLn $ "New client registered with PID: " <> show pid
  return pid

-- | Handle the endpoint that clients poll for published events.
--
-- This endpoint should be long-polled and will respond with a list of
-- zero or more events as they are available.
handleEvents :: ServerState -> PlayerId -> Handler (Seq Event)
handleEvents ServerState{..} pid = liftIO $ atomically $ do
  positions <- readTVar clientOffsets
  case Map.lookup pid positions of
    Nothing -> return mempty
    Just offset -> do
      history <- readTVar eventHistory
      modifyTVar' clientOffsets $ Map.insert pid (length history)
      return $ Seq.drop offset history

handleReset :: ServerState -> PlayerId -> Handler NoContent
handleReset ServerState{..} pid = liftIO . atomically $
  modifyTVar' clientOffsets (Map.insert pid 0) $> NoContent

handleHand :: ServerState -> PlayerId -> Handler [Tile]
handleHand ServerState{..} pid =
  tilesInZone (Hand pid)
  <$> liftIO (readTVarIO gameState)

handleBoard :: ServerState -> PlayerId -> Handler [Tile]
handleBoard ServerState{..} _ =
  tilesInZone Play
  <$> liftIO (readTVarIO gameState)

handleQueryMarker :: ServerState -> Company -> Handler (Maybe Tile)
handleQueryMarker ServerState{..} com =
  Map.lookup com
  . gameMarkers
  <$> liftIO (readTVarIO gameState)

--------------------------------------------------------------------------------
-- Handlers for Game Events

handleGameEvent
  :: ServerState
  -> Game a
  -> Event
  -> Handler a
handleGameEvent server handler event = do
  result <- join . liftIO . atomically $
    runGameSTM handler (gameState server) >>= \case
      Left err  -> return $ throwError $ gameErrorToServerError err
      Right res -> publish server event $> return res
  liftIO $ TIO.putStrLn $ "Event: " <> T.pack (show event)
  return result

handleDraw
  :: ServerState
  -> PlayerId
  -> Handler Tile
handleDraw server pid = handleGameEvent server (doDraw pid) (DrawEvent pid)

handleMove
  :: ServerState
  -> PlayerId
  -> Tile
  -> TileZone
  -> TileZone
  -> Handler NoContent
handleMove server pid tile src dst =
  handleGameEvent server (moveTile src dst tile) (MoveEvent pid tile src dst)
  $> NoContent

handleMarker
  :: ServerState
  -> PlayerId
  -> Company
  -> Maybe Tile
  -> Handler NoContent
handleMarker server pid com mtile =
  handleGameEvent server (doMarker pid com mtile) (MarkerEvent pid com mtile)
  $> NoContent

handleMoney
  :: ServerState
  -> PlayerId
  -> Int
  -> Handler NoContent
handleMoney server pid amount =
  handleGameEvent server (doMoney pid amount) (MoneyEvent pid amount)
  $> NoContent

handleStock
  :: ServerState
  -> PlayerId
  -> Company
  -> Int
  -> Handler NoContent
handleStock server pid com amount =
  handleGameEvent server (doStock pid com amount) (StockEvent pid com amount)
  $> NoContent
