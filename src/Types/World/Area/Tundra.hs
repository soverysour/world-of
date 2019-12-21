module Types.World.Area.Tundra
  ( Tundra(..)
  ) where

import           Protolude

import           Types.Common
import qualified Types.World as W

data Tundra =
  Tundra
    { elevation        :: Level
    , animalPopulation :: Level
    , forestation      :: Level
    , grassiness       :: Level
    }
  deriving (Eq, Show)

instance W.AreaClass Tundra where
  zoneOf _ = W.Tundra
  x `interpreting` _ = pure ([], W.WorldArea x)
