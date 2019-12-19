module Types.World.Tundra
  ( Tundra(..)
  ) where

import           ClassyPrelude

import           Types.Common

data Tundra =
  Tundra
    { elevation        :: Level
    , animalPopulation :: Level
    , forestation      :: Level
    , grassiness       :: Level
    }
  deriving (Eq, Show)
