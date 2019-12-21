module Types.World.Area.Mountain
  ( Mountain(..)
  ) where

import           Protolude

import           Types.Common
import qualified Types.World as W

data Mountain =
  Mountain
    { elevation        :: Level
    , animalPopulation :: Level
    , forestation      :: Level
    , rockiness        :: Level
    }
  deriving (Eq, Show)
  
instance W.AreaClass Mountain where
  zoneOf _ = W.Mountain
  x `interpreting` _ = pure ([], W.WorldArea x)
