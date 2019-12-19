module World.Config
  ( WorldConfig(..)
  , smallWorld
  , largeWorld
  ) where

import           ClassyPrelude

data WorldConfig =
  WorldConfig
    { sizeX :: Word
    , sizeY :: Word
    , zones :: Word
    }

smallWorld :: WorldConfig
smallWorld = WorldConfig 15 15 6

largeWorld :: WorldConfig
largeWorld = WorldConfig 40 40 15
