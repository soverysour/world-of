{-# LANGUAGE ExistentialQuantification #-}

module Types.World
  ( WorldZone(..)
  , World(..)
  , WorldArea(..)
  , AreaClass(..)
  ) where

import           ClassyPrelude
import qualified Data.Matrix   as M

import           GHC.Event     (Event)

-- All zones, except Water, can contain a human settlement.
data WorldZone
  = Water -- ^ Can be a river, lake, ocean
  | Mountain -- ^ Rocky, high, mountain
  | Normal -- ^ Normal elevation, grassy land, may contain forests.
  | Tundra -- ^ Cold, deserted land
  | Desert -- ^ Hot Desert
  | Swamp -- ^ Swamp, low elevation, forested
  | Rocky -- ^ Rocky, average elevation
  deriving (Eq, Show, Enum, Bounded)

data World =
  World
    { sizeX      :: Word
    , sizeY      :: Word
    , worldAreas :: M.Matrix WorldArea
    }
  deriving (Show)

data WorldArea =
  forall a. (Show a, AreaClass a) =>
            WorldArea a

instance Show WorldArea where
  show (WorldArea a) = show a

instance AreaClass WorldArea where
  zoneOf (WorldArea a) = zoneOf a
  (WorldArea a) `interpreting` event = a `interpreting` event

class AreaClass a where
  zoneOf :: a -> WorldZone
  interpreting :: a -> Event -> ([Event], WorldArea)
