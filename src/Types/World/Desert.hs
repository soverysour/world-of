module Types.World.Desert
  ( Desert(..)
  ) where

import           ClassyPrelude

import           Types.Common

data Desert =
  Desert
    { elevation        :: Level
    , animalPopulation :: Level
    , turbulence       :: Level
    }
  deriving (Eq, Show)
