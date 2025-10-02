{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy     (LazyByteString)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Network.Wai.Handler.Warp
import           Servant

import           Game
import           Types

type PidParam = QueryParam "pid" PlayerId
type TileParam = QueryParam "tile" Tile
type CompanyParam = QueryParam "company" Company
type AmountParam = QueryParam "amount" Int
type EventReq = Post '[JSON] NoContent

type RequireAPI = "register" :> Get '[JSON] PlayerId
  :<|> "events"  :> PidParam :> Get '[JSON] [Event]
  :<|> "draw"    :> PidParam :> Post '[JSON] Tile
  :<|> "play"    :> PidParam :> TileParam    :> EventReq
  :<|> "discard" :> PidParam :> TileParam    :> EventReq
  :<|> "marker"  :> PidParam :> CompanyParam :> TileParam :> EventReq
  :<|> "money"   :> PidParam :> AmountParam  :> EventReq
  :<|> "stock"   :> PidParam :> CompanyParam :> AmountParam :> EventReq

data ServerState = ServerState
  { pidSource  :: TVar PlayerId
  , sendQueues :: TVar (Map PlayerId (TQueue Event))
  , gameState  :: TVar GameState
  }

newServerState :: IO ServerState
newServerState =
  ServerState
  <$> newTVarIO 0
  <*> newTVarIO Map.empty
  <*> (newGameState >>= newTVarIO)

newPlayerId :: ServerState -> STM Int
newPlayerId ServerState{..} = modifyTVar' pidSource (+1) *> readTVar pidSource

-- | Append an 'Event' to the outgoing queue of every registered client.
broadcast :: ServerState -> Event -> STM ()
broadcast server event =
  readTVar (sendQueues server)
  >>= mapM_ (flip writeTQueue event)

-- | Handle the new client registration endpoint.
--
-- Generates a new player ID which is unique for this server and
-- allocates resources for managing this client.
handleRegister :: ServerState -> Handler PlayerId
handleRegister server = liftIO $ atomically $ do
  pid <- newPlayerId server
  queue <- newTQueue
  modifyTVar' (sendQueues server) (Map.insert pid queue)
  return pid

requireParam :: LazyByteString -> Maybe a -> Handler a
requireParam param = maybe (throwError err) pure
  where
    err = err400 { errBody = "\"" <> param <> "\"" <> " parameter is required" }

handleDraw :: ServerState -> Maybe PlayerId -> Handler Tile
handleDraw server mpid = do
  pid <- requireParam "pid" mpid
  result <- liftIO $ atomically $ runGameSTM (doDraw pid) (gameState server)
  case result of
    Left err   -> throwError $ gameErrorToServerError err
    Right tile -> return tile

handlePlay :: ServerState -> Maybe PlayerId -> Maybe Tile -> Handler NoContent
handlePlay server mpid mtile = do
  pid <- requireParam "pid" mpid
  tile <- requireParam "tile" mtile
  result <- liftIO $ atomically $ runGameSTM (doPlay pid tile) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent

handleDiscard :: ServerState -> Maybe PlayerId -> Maybe Tile -> Handler NoContent
handleDiscard server mpid mtile = do
  pid <- requireParam "pid" mpid
  tile <- requireParam "tile" mtile
  result <- liftIO $ atomically $ runGameSTM (doDiscard pid tile) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent

handleMarker :: ServerState -> Maybe PlayerId -> Maybe Company -> Maybe Tile -> Handler NoContent
handleMarker server mpid mcom mtile = do
  pid <- requireParam "pid" mpid
  com <- requireParam "company" mcom
  result <- liftIO $ atomically $ runGameSTM (doMarker pid com mtile) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent


handleMoney :: ServerState -> Maybe PlayerId -> Maybe Int -> Handler NoContent
handleMoney server mpid mamount= do
  pid <- requireParam "pid" mpid
  amount <- requireParam "amount" mamount
  result <- liftIO $ atomically $ runGameSTM (doMoney pid amount) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent


handleStock :: ServerState -> Maybe PlayerId -> Maybe Company -> Maybe Int -> Handler NoContent
handleStock server mpid mcom mamount= do
  pid <- requireParam "pid" mpid
  com <- requireParam "company" mcom
  amount <- requireParam "amount" mamount
  result <- liftIO $ atomically $ runGameSTM (doStock pid com amount) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent

-- | Handle the endpoint that clients poll for published events.
--
-- This endpoint should be long-polled and will respond with a list of
-- zero or more events as they are available.
handleEvents :: ServerState -> Maybe PlayerId -> Handler [Event]
handleEvents = undefined

requireServer :: ServerState -> Server RequireAPI
requireServer s =
  handleRegister s
  :<|> handleEvents s
  :<|> handleDraw s
  :<|> handlePlay s
  :<|> handleDiscard s
  :<|> handleMarker s
  :<|> handleMoney s
  :<|> handleStock s

requireAPI :: Proxy RequireAPI
requireAPI = Proxy

runServer :: ServerState -> IO ()
runServer = run 11073 . serve requireAPI . requireServer
