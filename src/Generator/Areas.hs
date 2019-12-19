module Generator.Areas
  ( mkMapAreaIO
  , mkMapArea
  ) where

import           ClassyPrelude
import           Control.Monad.Trans.State.Lazy
import           System.Random

import           Intro
import           Types.World
import qualified Types.World.Water as W

--------------------------------------------------------------------------------
--       Transform the matrix into another one using existential-types!       --
--------------------------------------------------------------------------------
mkMapAreaIO :: WorldZone -> IO WorldArea
mkMapAreaIO zone = do
  stdGen <- getStdGen
  let (area, g) = mkMapArea zone stdGen
  setStdGen g
  return area

mkMapArea :: WorldZone -> StdGen -> (WorldArea, StdGen)
mkMapArea zone gen =
  let areaStateT = mkMapArea' zone
   in runState areaStateT gen

mkMapArea' :: WorldZone -> State StdGen WorldArea
mkMapArea' _ = do
  salty <- getRandomBounded
  animals <- getRandomBounded
  depth <- getRandomBounded
  turbulence <- getRandomBounded
  return . WorldArea $ W.Water salty animals depth turbulence
