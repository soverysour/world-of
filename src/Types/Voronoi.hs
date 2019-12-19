module Types.Voronoi
  ( Voronoi(..)
  ) where

import           ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Matrix        as M

import           Types.Common

data Voronoi a =
  Voronoi
    { cells   :: M.Matrix a
    , dimx    :: DimX
    , dimy    :: DimY
    , centers :: NE.NonEmpty (PointWithInfo a)
    }
  deriving (Eq, Show)
