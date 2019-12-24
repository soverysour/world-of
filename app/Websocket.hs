module Websocket
  ( webSocketServer
  ) where

import           Control.Concurrent.Chan
import qualified Network.WebSockets      as WS
import           Control.Concurrent.MVar
import           Protolude

import           Config
import           Message

webSocketServer :: Chan OutToInEvent -> ServerConfig -> IO ()
webSocketServer chan (ServerConfig host port) = do
 _ <- newMVar []
 WS.runServer host port handle'
  where
    handle' pending = do
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (return ()) $ do
        msg <- WS.receiveData conn
        putStrLn (msg :: Text)
        chan `writeChan` GetWorld
