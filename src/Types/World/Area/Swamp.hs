module Types.World.Area.Swamp
  ( Swamp(..)
  ) where

import           Protolude

import           Types.Common
import qualified Types.World as W

data Swamp =
  Swamp
    { animalPopulation :: Level
    , depth            :: Level
    , forestation      :: Level
    }
  deriving (Eq, Show)

instance W.AreaClass Swamp where
  zoneOf _ = W.Swamp
  x `interpreting` _ = pure ([], W.WorldArea x)
