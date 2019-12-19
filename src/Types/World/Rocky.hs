module Types.World.Rocky
  ( Rocky(..)
  ) where

import           ClassyPrelude

import           Types.Common

data Rocky =
  Rocky
    { animalPopulation :: Level
    , elevation        :: Level
    }
  deriving (Eq, Show)
