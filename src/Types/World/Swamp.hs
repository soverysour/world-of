module Types.World.Swamp
  ( Swamp(..)
  ) where

import           ClassyPrelude

import           Types.Common

data Swamp =
  Swamp
    { animalPopulation :: Level
    , depth            :: Level
    , forestation      :: Level
    }
  deriving (Eq, Show)
