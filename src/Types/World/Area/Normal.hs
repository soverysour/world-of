module Types.World.Area.Normal
  ( Normal(..)
  ) where

import           Protolude

import           Types.Common
import qualified Types.World as W

data Normal =
  Normal
    { elevation        :: Level
    , animalPopulation :: Level
    , forestation      :: Level
    , grassiness       :: Level
    }
  deriving (Eq, Show)

instance W.AreaClass Normal where
  zoneOf _ = W.Normal
  x `interpreting` _ = pure ([], W.WorldArea x)
