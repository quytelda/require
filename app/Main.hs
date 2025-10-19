{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent.Async
import qualified Data.Text.Lazy.Builder.Int as TBI

import           Server
import           Types

main :: IO ()
main = do
  server <- newServerState
  putBuilderLn
    $ "Starting new server on port " <> TBI.decimal (serverPort server)
    <> " with server ID: " <> TBI.decimal (serverId server)
  concurrently_
    (runServer server)
    (logEvents server)
