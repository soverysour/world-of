module Message
  ( OutToInEvent(..)
  , InToOutEvent(..)
  , TickerMsg(..)
  , SimulationRequest(..)
  , SimulationResult(..)
  , WebSocketMessageIn(..)
  , WebSocketMessageOut(..)
  ) where

import           Data.Aeson
import           Protolude

data OutToInEvent
  = GetWorld
  | NoEvents
  deriving (Eq, Show, Enum, Bounded)

data InToOutEvent
  = HeresWorld
  | Nothing
  deriving (Eq, Show, Enum, Bounded)

data TickerMsg
  = Tick
  deriving (Eq, Show, Enum, Bounded)

data SimulationRequest =
  Whatever2
  deriving (Eq, Show, Enum, Bounded)

data SimulationResult =
  Whatever
  deriving (Eq, Show, Enum, Bounded)

data WebSocketMessageIn
  = Register Text
  | Get
  deriving (Eq, Show, Generic)

data WebSocketMessageOut
  = NameAlreadyExists
  | BadMessage
  deriving (Eq, Show, Generic)

instance FromJSON WebSocketMessageIn

instance ToJSON WebSocketMessageOut
