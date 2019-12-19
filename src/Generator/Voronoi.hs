module Generator.Voronoi
  ( mkVoronoiIO
  , mkVoronoi
  ) where

import           ClassyPrelude
import           Control.Monad.Trans.State.Lazy
import qualified Data.List.NonEmpty             as NE
import qualified Data.Matrix                    as M
import qualified Data.Set                       as Set
import           System.Random

import           Intro
import           Orphanage                      ()
import           Types.Common
import           Types.Voronoi

mkVoronoiIO :: (Enum a, Bounded a) => DimX -> DimY -> Count -> IO (Voronoi a)
mkVoronoiIO dimx dimy count = do
  stdGen <- getStdGen
  let (voronoi, g) = mkVoronoi dimx dimy count stdGen
  setStdGen g
  return voronoi

mkVoronoi :: (Enum a, Bounded a) => DimX -> DimY -> Count -> StdGen -> (Voronoi a, StdGen)
mkVoronoi dimx dimy count gen =
  let voronoiStateT = mkVoronoi' dimx dimy count
   in runState voronoiStateT gen

-- | Generates a voronoi diagram by randomly and safely generating enum values.
-- | The bounds are 1 indexed and therefore the upper bounds are inclusive.
mkVoronoi' :: (Enum a, Bounded a) => DimX -> DimY -> Count -> State StdGen (Voronoi a)
mkVoronoi' dimx dimy randomPoints = do
  centers <- take (fromIntegral randomPoints) <$> getRandomInf
  (_, positionedCenters) <- foldM (combineF dimx dimy) (Set.empty, []) centers
  let positionedCenters' =
        case positionedCenters of
          []     -> ((0, 0), 0) NE.:| []
          -- ^ Fill the map with a default enum - a value of 0 must exist, else the type can't be an enum.
          (x:xs) -> x NE.:| xs
  let labels = fmap toWrappedEnum <$> positionedCenters'
      cells = M.matrix (fromIntegral dimy) (fromIntegral dimx) (\(y, x) -> findNearest x y labels)
  return $ Voronoi cells dimx dimy labels

findNearest :: Int -> Int -> NE.NonEmpty (PointWithInfo a) -> a
findNearest x y points =
  let points' =
        NE.sortBy (\(d1, _) (d2, _) -> compare d1 d2) $
        (\((x', y'), l) -> (abs (x - fromIntegral x') + abs (y - fromIntegral y'), l)) <$> points
   in snd $ NE.head points'

combineF :: DimX -> DimY -> (Set Point, [LabeledPoint]) -> Label -> State StdGen (Set Point, [LabeledPoint])
combineF dimx dimy (set, acc) label = do
  point' <- getFreePoint dimx dimy set
  let set' = point' `Set.insert` set
  return (set', (point', label) : acc)

getFreePoint :: DimX -> DimY -> Set Point -> State StdGen Point
getFreePoint dimx dimy set = do
  (x, y) <- getRandom
  let point' = ((x `mod` dimx) + 1, (y `mod` dimy) + 1)
  if point' `Set.member` set
    then getFreePoint dimx dimy set
    else return point'
