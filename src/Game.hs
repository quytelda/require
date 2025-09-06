{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import           Conduit
import           Control.Concurrent.STM
import           Control.Exception
import           Data.Conduit.Network
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Protocol
import           Types

handleEvent :: MonadThrow m => Event -> GameMonad m ()
handleEvent (DrawEvent pid _)           = grabFromPool
                                          >>= setTileStatus (Hand pid)
handleEvent (PlayEvent pid tile)        = checkHasTile pid tile
                                          *> setTileStatus Play tile
handleEvent (DiscardEvent pid tile)     = checkHasTile pid tile
                                          *> setTileStatus Discard tile
handleEvent (ReturnEvent pid tile)      = checkHasTile pid tile
                                          *> setTileStatus Pool tile
handleEvent (MarkerEvent _ com mtile)   = setMarker com mtile
handleEvent (MoneyEvent pid amount)     = transferMoney pid amount
handleEvent (StockEvent pid com amount) = transferStock pid com amount
handleEvent _                           = pure ()

handleEvents :: MonadThrow m => ConduitT Event o (GameMonad m) ()
handleEvents = awaitForever $ lift . handleEvent
