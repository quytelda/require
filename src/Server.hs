{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Server where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Network.Wai.Handler.Warp
import           Servant

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
  }

newServerState :: IO ServerState
newServerState =
  ServerState
  <$> newTVarIO 0
  <*> newTVarIO Map.empty

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

handleDraw :: ServerState -> Maybe PlayerId -> Handler Tile
handleDraw = undefined

handlePlay :: ServerState -> Maybe PlayerId -> Maybe Tile -> Handler NoContent
handlePlay = undefined

handleDiscard :: ServerState -> Maybe PlayerId -> Maybe Tile -> Handler NoContent
handleDiscard = undefined

handleMarker :: ServerState -> Maybe PlayerId -> Maybe Company -> Maybe Tile -> Handler NoContent
handleMarker = undefined

handleMoney :: ServerState -> Maybe PlayerId -> Maybe Int -> Handler NoContent
handleMoney = undefined

handleStock :: ServerState -> Maybe PlayerId -> Maybe Company -> Maybe Int -> Handler NoContent
handleStock = undefined

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
