{-# LANGUAGE ExistentialQuantification #-}

module Types.World
  ( World(..)
  , WorldZone(..)
  , WorldArea(..)
  , WorldActor(..)
  , WorldEvent(..)
  , AreaClass(..)
  , ActorClass(..)
  , Pulse(..)
  , Pulser
  , timeEvent
  , Indexed(..)
  , Timed(..)
  ) where

import qualified Data.Matrix     as M
import qualified Data.SortedList as SL
import           GHC.Show        (Show (..))
import           Protolude       hiding (show)

import           Types.Common

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
    { sizeX       :: Word
    , sizeY       :: Word
    , worldAreas  :: M.Matrix WorldArea
    , worldActors :: [Indexed WorldActor]
    , events      :: SL.SortedList (Timed (Indexed WorldEvent))
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

data WorldActor =
  forall a. (Show a, ActorClass a) =>
            WorldActor a

instance Show WorldActor where
  show (WorldActor a) = show a

instance ActorClass WorldActor where
  (WorldActor a) `acting` eventAt = a `acting` eventAt

-- Core difference between Area and Actor is that Area is static - they don't change positions
-- and they can't affect a different position.
-- This is reflected in the event handling functions.
class AreaClass a where
  zoneOf :: a -> WorldZone
  interpreting :: Monad m => a -> Pulser -> WithRandT m ([WorldEvent], WorldArea)

class ActorClass a where
  acting :: Monad m => a -> Pulser -> WithRandT m ([WorldEvent], [WorldActor])

data WorldEvent
  = PopulationDecrease
  | NoEvent
  deriving (Eq, Show)

timeEvent :: WorldEvent -> Word
timeEvent NoEvent            = 0
timeEvent PopulationDecrease = 0

data Pulse =
  Pulse
  deriving (Eq, Show, Enum, Bounded)

type Pulser = Either Pulse WorldEvent

-- Specific type of element - enriched with time, location target, and generator id.
-- Eq and Ord just by time.
data Indexed a =
  Indexed
    { unElement :: a
    , unPoint   :: Point
    , unKey     :: Key
    }
  deriving (Eq, Show)

data Timed a =
  Timed
    { a      :: a
    , unTime :: Integer
    }
  deriving (Show)

instance Eq (Timed a) where
  i1 == i2 = unTime i1 == unTime i2

instance Ord (Timed a) where
  i1 `compare` i2 = unTime i1 `compare` unTime i2
