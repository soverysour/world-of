module Types.World.Area.Rocky
  ( Rocky(..)
  ) where

import           Protolude

import           Types.Common
import qualified Types.World as W

data Rocky =
  Rocky
    { animalPopulation :: Level
    , elevation        :: Level
    }
  deriving (Eq, Show)

instance W.AreaClass Rocky where
  zoneOf _ = W.Rocky
  x `interpreting` _ = pure ([], W.WorldArea x)
