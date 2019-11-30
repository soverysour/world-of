module Voronoi
  ( Voronoi(..)
  , mkVoronoiIO
  ) where

import           ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set
import qualified Data.Vector        as V
import           System.Random

import           Orphanage

data Voronoi a =
  Voronoi
    { cells :: Vector (Vector a)
    , dimx  :: DimX
    , dimy  :: DimY
    }
  deriving (Eq, Show)

-- TODO: Split mkVoronoiIO into two, one using IO, one using a random number generator.
-- | Generates a voronoi diagram by randomly and safely generating enum values.
-- | The bounds are 0 indexed and therefore the upper bounds are exclusive.
mkVoronoiIO :: (Enum a, Bounded a) => DimX -> DimY -> Count -> IO (Voronoi a)
mkVoronoiIO dimx dimy randomPoints = do
  centers <- take (fromIntegral randomPoints) . randoms <$> getStdGen
  (_, positionedCenters) <- foldM (combineF dimx dimy) (Set.empty, []) centers
  let positionedCenters' =
        case positionedCenters of
          []     -> ((0, 0), 0) NE.:| []
          -- ^ Fill the map with a default enum - a value of 0 must exist, else the type can't be an enum.
          (x:xs) -> x NE.:| xs
  let labels = fmap toWrappedEnum <$> positionedCenters'
      cells =
        V.generate
          (fromIntegral dimy)
          (\y -> V.generate (fromIntegral dimx) (\x -> findNearest x y labels))
  return $ Voronoi cells dimx dimy

findNearest :: Int -> Int -> NE.NonEmpty (PointWithInfo a) -> a
findNearest x y points =
  let points' =
        NE.sortBy (\(d1, _) (d2, _) -> compare d1 d2) $
        (\((x', y'), l) ->
           (abs (x - fromIntegral x') + abs (y - fromIntegral y'), l)) <$>
        points
   in snd $ NE.head points'

combineF ::
     DimX
  -> DimY
  -> (Set Point, [LabeledPoint])
  -> Label
  -> IO (Set Point, [LabeledPoint])
combineF dimx dimy (set, acc) label = do
  point' <- getFreePoint dimx dimy set
  let set' = point' `Set.insert` set
  return (set', (point', label) : acc)

getFreePoint :: DimX -> DimY -> Set Point -> IO Point
getFreePoint dimx dimy set = do
  (x, y) <- randomIO
  let point' = (x `mod` dimx, y `mod` dimy)
  if point' `Set.member` set
    then getFreePoint dimx dimy set
    else return point'

type Count = Word

type Dim = Word

type DimX = Dim

type DimY = Dim

type Label = Word

type Pos = Word

type Point = (Pos, Pos)

type PointWithInfo a = (Point, a)

type LabeledPoint = PointWithInfo Label
