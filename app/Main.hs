module Main (main) where

import           Server
import           Types

main :: IO ()
main = newServerState >>= runServer
