module Websocket
  ( webSocketServer
  ) where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.Map.Strict         as M
import qualified Network.WebSockets      as WS
import           Protolude

import           Config
import           Message

type Client = (Text, WS.Connection)

type Clients = M.Map Text WS.Connection

webSocketServer :: Chan OutToInEvent -> Chan InToOutEvent -> ServerConfig -> IO ()
webSocketServer chanToInside chanToOutside (ServerConfig host port) = do
  clients <- newMVar M.empty
  void $ forkIO $ forever $ handleResponse chanToOutside
  WS.runServer host port (handle' clients chanToInside)

handle' :: MVar Clients -> Chan OutToInEvent -> WS.ServerApp
handle' clients chan pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ acting clients chan conn

acting :: MVar Clients -> Chan OutToInEvent -> WS.Connection -> IO ()
acting clients chan conn = do
  msg <- decode <$> WS.receiveData conn
  let retryWith a = WS.sendBinaryData conn (encode a) >> acting clients chan conn
  case msg of
    Just (Register username) -> do
      let client = (username, conn)
      ifM
        (isJust . (M.!? username) <$> readMVar clients)
        (retryWith NameAlreadyExists)
        (do modifyMVar_ clients $ pure . M.insert username conn
            finally (loop chan client clients) (disconnect client clients))
    _ -> retryWith BadMessage

loop :: Chan OutToInEvent -> Client -> MVar Clients -> IO ()
loop chan client@(_, conn) clients = do
  msg <- decode <$> WS.receiveData conn
  case msg of
    Just Get -> do
      print ("Got Get." :: Text)
      loop chan client clients
    _ -> WS.sendBinaryData conn (encode BadMessage) >> loop chan client clients

disconnect :: Client -> MVar Clients -> IO ()
disconnect (username, _) clients = modifyMVar_ clients (pure . M.delete username)

handleResponse :: Chan InToOutEvent -> IO ()
handleResponse chan = do
  _ <- readChan chan
  putStrLn ("Handled a response" :: Text)
