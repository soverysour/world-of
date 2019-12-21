module Types.World.Area.Water
  ( Water(..)
  ) where

import           Protolude

import           Types.Common
import qualified Types.World as W

data Water =
  Water
    { isSalty          :: Bool
    , animalPopulation :: Level
    , depth            :: Level
    , turbulence       :: Level
    }
  deriving (Show, Eq)

instance W.AreaClass Water where
  zoneOf _ = W.Water
  x `interpreting` _ = pure ([], W.WorldArea x)
