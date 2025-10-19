{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Server (runServer) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Data.Functor
import           Data.IntMap.Strict         (IntMap)
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
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
  -- State Queries
  =    "serverid" :> Get '[JSON] ServerId
  :<|> "state"    :> Get '[JSON] GameState
  :<|> "players"  :> Get '[JSON] (IntMap Text)

  -- Event Broadcast
  :<|> "events"
       :> QueryParam "start" Int
       :> QueryParam "end" Int
       :> EventGet EventRecord
  :<|> "history"
       :> Capture "index" Int
       :> Get '[JSON] Event

  -- Event Requests
  :<|> "join"
       :> QueryParam "name" Text
       :> Post '[JSON] PlayerId
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
          :<|> "setup"
            :> Post '[JSON] [Tile]
          )

requireServer :: ServerState -> Server RequireAPI
requireServer s =
  handleServerId s
  :<|> handleState s
  :<|> handlePlayers s
  :<|> handleEvents s
  :<|> handleHistory s
  :<|> handleJoin s
  :<|> (\pid -> handleDraw s pid
         :<|> handleMove s pid
         :<|> handleMarker s pid
         :<|> handleMoney s pid
         :<|> handleStock s pid
         :<|> handleSetup s pid
       )

requireAPI :: Proxy RequireAPI
requireAPI = Proxy

runServer :: ServerState -> IO ()
runServer server = run (serverPort server) . serve requireAPI . requireServer $ server

--------------------------------------------------------------------------------
-- Servant Endpoint Handlers

-- | An endpoint to retrieve the server's ID.
--
-- Clients should store this ID alongside their own player ID so that
-- when a new server is created, they will be able to tell that the
-- player ID has expired because the server ID no longer matches.
handleServerId :: ServerState -> Handler ServerId
handleServerId ServerState{..} = return serverId

handleState :: ServerState -> Handler GameState
handleState ServerState{..} = liftIO $ readTVarIO gameState

handlePlayers :: ServerState -> Handler (IntMap Text)
handlePlayers ServerState{..} = liftIO $ gamePlayers <$> readTVarIO gameState

handleEvents
  :: ServerState
  -> Maybe Int
  -> Maybe Int
  -> Handler (EventStream EventRecord)
handleEvents server mstart mend =
  let start = fromMaybe 0 mstart
      end = fromMaybe (start + 8) mend
  in return $ sourceToEventStream $ sourceHistoryRange server start end

handleHistory
  :: ServerState
  -> Int
  -> Handler Event
handleHistory server index = do
  result <- liftIO $ race
    (threadDelay delay)
    (nthEvent server index)
  case result of
    Right event -> return event
    Left ()     -> throwError timeoutError
  where
    timeoutError = ServerError
      { errHTTPCode = 408
      , errReasonPhrase = "Request Timeout"
      , errBody = "The requested event does not exist yet."
      , errHeaders = []
      }
    delay = 30 * 1000000 -- 30sec in microseconds

--------------------------------------------------------------------------------
-- Handlers for Game Event Requests

-- | Handle the new client registration endpoint.
--
-- Generates a new player ID which is unique for this server and
-- allocates resources for managing this client.
handleJoin :: ServerState -> Maybe Text -> Handler PlayerId
handleJoin server mname = liftIO $ atomically $ do
  pid <- newPlayerId server
  let name = fromMaybe (defaultName pid) mname
  runGameAction server (doJoin pid name)
  return pid
  where
    defaultName pid =
      TL.toStrict
      $ TB.toLazyText
      $ "Player " <> TBI.decimal pid

handleGameAction
  :: ServerState
  -> GameAction a
  -> Handler a
handleGameAction server =
  join
  . liftIO
  . atomically
  . flip catchSTM (pure . throwError . gameErrorToServerError)
  . fmap pure
  . runGameAction server

handleDraw
  :: ServerState
  -> PlayerId
  -> Handler Tile
handleDraw server pid = do
  tile <- handleGameAction server (doDraw pid)
  liftIO $ putBuilderLn
    $  "Tile " <> renderTile tile
    <> " drawn by player " <> TBI.decimal pid
  return tile

handleMove
  :: ServerState
  -> PlayerId
  -> Tile
  -> TileZone
  -> TileZone
  -> Handler NoContent
handleMove server pid tile src dst =
  handleGameAction server (doMove pid src dst tile)
  $> NoContent

handleMarker
  :: ServerState
  -> PlayerId
  -> Company
  -> Maybe Tile
  -> Handler NoContent
handleMarker server pid com mtile =
  handleGameAction server (doMarker pid com mtile)
  $> NoContent

handleMoney
  :: ServerState
  -> PlayerId
  -> Int
  -> Handler NoContent
handleMoney server pid amount =
  handleGameAction server (doMoney pid amount)
  $> NoContent

handleStock
  :: ServerState
  -> PlayerId
  -> Company
  -> Int
  -> Handler NoContent
handleStock server pid com amount =
  handleGameAction server (doStock pid com amount)
  $> NoContent

handleSetup
  :: ServerState
  -> PlayerId
  -> Handler [Tile]
handleSetup server pid = do
  handleMoney server pid 6000
  replicateM 6 (handleDraw server pid)
