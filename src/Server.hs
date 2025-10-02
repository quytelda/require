{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Network.Wai.Handler.Warp
import           Servant

import           Game
import           Types

type RequiredParam = QueryParam' '[Required, Strict]
type TileParam = RequiredParam "tile" Tile
type CompanyParam = RequiredParam "company" Company
type AmountParam = RequiredParam "amount" Int
type EventReq = Post '[JSON] NoContent

type RequireAPI = "register" :> Get '[JSON] PlayerId
  :<|> Capture "PlayerId" PlayerId :> "events" :> Get '[JSON] [Event]
  :<|> Capture "PlayerId" PlayerId
  :> (    "draw"    :> Post '[JSON] Tile
     :<|> "play"    :> TileParam    :> EventReq
     :<|> "discard" :> TileParam    :> EventReq
     :<|> "marker"  :> CompanyParam :> QueryParam "tile" Tile :> EventReq
     :<|> "money"   :> AmountParam  :> EventReq
     :<|> "stock"   :> CompanyParam :> AmountParam :> EventReq
     )

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

-- | Handle the endpoint that clients poll for published events.
--
-- This endpoint should be long-polled and will respond with a list of
-- zero or more events as they are available.
handleEvents :: ServerState -> PlayerId -> Handler [Event]
handleEvents = undefined

handleDraw :: ServerState -> PlayerId -> Handler Tile
handleDraw server pid = do
  result <- liftIO $ atomically $ runGameSTM (doDraw pid) (gameState server)
  case result of
    Left err   -> throwError $ gameErrorToServerError err
    Right tile -> return tile

handlePlay :: ServerState -> PlayerId -> Tile -> Handler NoContent
handlePlay server pid tile = do
  result <- liftIO $ atomically $ runGameSTM (doPlay pid tile) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent

handleDiscard :: ServerState -> PlayerId -> Tile -> Handler NoContent
handleDiscard server pid tile = do
  result <- liftIO $ atomically $ runGameSTM (doDiscard pid tile) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent

handleMarker :: ServerState -> PlayerId -> Company -> Maybe Tile -> Handler NoContent
handleMarker server pid com mtile = do
  result <- liftIO $ atomically $ runGameSTM (doMarker pid com mtile) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent

handleMoney :: ServerState -> PlayerId -> Int -> Handler NoContent
handleMoney server pid amount = do
  result <- liftIO $ atomically $ runGameSTM (doMoney pid amount) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent

handleStock :: ServerState -> PlayerId -> Company -> Int -> Handler NoContent
handleStock server pid com amount= do
  result <- liftIO $ atomically $ runGameSTM (doStock pid com amount) (gameState server)
  case result of
    Left err -> throwError $ gameErrorToServerError err
    Right () -> return NoContent

requireServer :: ServerState -> Server RequireAPI
requireServer s =
  handleRegister s
  :<|> handleEvents s
  :<|> (\pid -> handleDraw s pid
         :<|> handlePlay s pid
         :<|> handleDiscard s pid
         :<|> handleMarker s pid
         :<|> handleMoney s pid
         :<|> handleStock s pid
       )

requireAPI :: Proxy RequireAPI
requireAPI = Proxy

runServer :: ServerState -> IO ()
runServer = run 11073 . serve requireAPI . requireServer
