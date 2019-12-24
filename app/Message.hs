module Message
  ( OutToInEvent(..)
  , InToOutEvent(..)
  , TickerMsg(..)
  , SimulationRequest(..)
  , SimulationResult(..)
  ) where

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
  = StartTick
  | EndTick
  deriving (Eq, Show, Enum, Bounded)

data SimulationRequest
  = Whatever2
  deriving (Eq, Show, Enum, Bounded)

data SimulationResult =
  Whatever
  deriving (Eq, Show, Enum, Bounded)
