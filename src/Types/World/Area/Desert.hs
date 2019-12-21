module Types.World.Area.Desert
  ( Desert(..)
  ) where

import           Protolude

import           Types.Common
import qualified Types.World as W

data Desert =
  Desert
    { elevation        :: Level
    , animalPopulation :: Level
    , turbulence       :: Level
    }
  deriving (Eq, Show)

instance W.AreaClass Desert where
  zoneOf _ = W.Desert
  x `interpreting` _ = pure ([], W.WorldArea x)
