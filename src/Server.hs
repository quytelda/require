{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import           Servant

import           Types

type RequireAPI = "register" :> Get '[JSON] PlayerId
  :<|> "event" :> Capture "playerId" Int :> "push"
               :> ReqBody '[JSON] Event :> Post '[JSON] NoContent
  :<|> "event" :> Capture "playerId" Int :> "pull"
               :> Get  '[JSON] [Event]
