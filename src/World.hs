module World
  ( createWorld
  , WorldConfig(..)
  ) where

import           ClassyPrelude

import           Generator.Areas
import           Generator.Voronoi
import           Types.Voronoi
import           Types.World
import           World.Config

createWorld :: WorldConfig -> IO World
createWorld (WorldConfig sizeX sizeY zones) = do
  roughMap <- mkVoronoiIO sizeX sizeY zones
  mapAreas <- traverse mkMapAreaIO $ cells roughMap
  return $ World sizeX sizeY mapAreas
