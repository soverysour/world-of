module Types.World.Mountain
  ( Mountain(..)
  ) where

import           ClassyPrelude

import           Types.Common

data Mountain =
  Mountain
    { elevation        :: Level
    , animalPopulation :: Level
    , forestation      :: Level
    , rockyness        :: Level
    }
  deriving (Eq, Show)
