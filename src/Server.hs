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

type RequireAPI = "register" :> Get '[JSON] PlayerId
  :<|> "event" :> Capture "playerId" Int :> "push"
               :> ReqBody '[JSON] Event :> Post '[JSON] NoContent
  :<|> "event" :> Capture "playerId" Int :> "pull"
               :> Get  '[JSON] [Event]

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

-- | Handle the endpoint clients use to send new events.
--
-- Receive an event from the client and attempt to apply it to the
-- game state. On success, we publish the event and respond with a
-- simple 200 response; otherwise, we respond with some error.
handleEventPush :: ServerState -> PlayerId -> Event -> Handler NoContent
handleEventPush = undefined

-- | Handle the endpoint that clients poll for published events.
--
-- This endpoint should be long-polled and will respond with a list of
-- zero or more events as they are available.
handleEventPull :: ServerState -> PlayerId -> Handler [Event]
handleEventPull = undefined

requireServer :: ServerState -> Server RequireAPI
requireServer s = handleRegister s :<|> handleEventPush s :<|> handleEventPull s

requireAPI :: Proxy RequireAPI
requireAPI = Proxy

runServer :: ServerState -> IO ()
runServer = run 11073 . serve requireAPI . requireServer
