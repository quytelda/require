{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.Text.Lazy.Builder.Int as TBI
import           System.Console.GetOpt
import           System.Environment

import           Game.Internal
import           Server
import           Server.Types

options :: [OptDescr (ServerState -> ServerState)]
options =
  [ Option "p" ["port"]
    (ReqArg (\cs server -> server {serverPort = read cs}) "PORT")
    "The TCP port the server will listen on (default 11073)"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (fs, _, errs) = getOpt RequireOrder options args
      applyOptions = foldl (.) id fs

  unless (null errs) $
    mapM_ putStrLn errs

  server <- applyOptions <$> newServerState
  putBuilderLn
    $ "Starting new server on port " <> TBI.decimal (serverPort server)
    <> " with server ID: " <> TBI.decimal (serverId server)
  concurrently_
    (runServer server)
    (logEvents server)
