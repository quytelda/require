{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Data.Functor
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Data.Text.Lazy.Builder.Int as TBI

import           Network.Wai.Handler.Warp
import           Servant

import           Game
import           SSE
import           Types

type RequiredParam = QueryParam' '[Required, Strict]
type TileParam = RequiredParam "tile" Tile
type CompanyParam = RequiredParam "company" Company
type AmountParam = RequiredParam "amount" Int
type EventReq = Post '[JSON] NoContent

type RequireAPI
  =    "serverid" :> Get '[JSON] ServerId
  :<|> "events"
       :> QueryParam "start" Int
       :> QueryParam "end" Int
       :> EventGet EventRecord

  -- State Queries
  :<|> "state"   :> Get '[JSON] GameState
  :<|> "players" :> Get '[JSON] [PlayerId]

  -- Event Queries
  :<|> "join" :> Post '[JSON] PlayerId
  :<|> Capture "PlayerId" PlayerId
       :> (    "draw"
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
  :<|> handleRange s
  :<|> handleState s
  :<|> handlePlayers s
  :<|> handleJoin s
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
handleJoin :: ServerState -> Handler PlayerId
handleJoin server = liftIO $ do
  pid <- atomically $ do
    pid <- newPlayerId server
    modifyTVar' (clientOffsets server) (Map.insert pid 0)
    publish server (JoinEvent pid)
    return pid
  putBuilderLn $ "New client allocated with PlayerID: " <> TBI.decimal pid
  putBuilderLn $ displayEvent $ JoinEvent pid
  return pid

handleRange
  :: ServerState
  -> Maybe Int
  -> Maybe Int
  -> Handler (EventStream EventRecord)
handleRange server mstart mend =
  let start = fromMaybe 0 mstart
      end = fromMaybe (start + 8) mend
  in return $ sourceToEventStream $ sourceHistoryRange server start end

handleState :: ServerState -> Handler GameState
handleState ServerState{..} = liftIO $ readTVarIO gameState

handlePlayers :: ServerState -> Handler [PlayerId]
handlePlayers ServerState{..} = liftIO $ Map.keys <$> readTVarIO clientOffsets

--------------------------------------------------------------------------------
-- Handlers for Game Events

handleGameEvent
  :: ServerState
  -> Game a
  -> Event
  -> Handler a
handleGameEvent server handler event =
  stmToHandler (runGame handler (gameState server) <* publish server event)
  <* (liftIO . putBuilderLn . displayEvent) event
  where
    stmToHandler =
      join
      . liftIO
      . atomically
      . flip catchSTM (pure . throwError . gameErrorToServerError)
      . fmap pure

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
