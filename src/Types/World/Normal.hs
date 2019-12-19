module Types.World.Normal
  ( Normal(..)
  ) where

import           ClassyPrelude

import           Types.Common

data Normal =
  Normal
    { elevation        :: Level
    , animalPopulation :: Level
    , forestation      :: Level
    , grassiness       :: Level
    }
  deriving (Eq, Show)
