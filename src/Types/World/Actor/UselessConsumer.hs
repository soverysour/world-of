module Types.World.Actor.UselessConsumer
  ( UselessConsumer(..)
  , mkUselessConsumer
  ) where

import           Protolude

import           Types.World

mkUselessConsumer :: WorldActor
mkUselessConsumer = WorldActor $ UselessConsumer True

newtype UselessConsumer =
  UselessConsumer
    { hungry :: Bool
    }
  deriving (Eq, Show)

instance ActorClass UselessConsumer where
  acting consumer pulseOrEvent =
    case pulseOrEvent of
      Left Pulse ->
        pure $
        if hungry consumer
          then ([PopulationDecrease], [WorldActor $ consumer {hungry = False}])
          else ([], [WorldActor $ consumer {hungry = True}])
      _ -> pure ([], [WorldActor consumer])
