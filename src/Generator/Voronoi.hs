module Generator.Voronoi
  ( mkVoronoiIO
  , mkVoronoi
  ) where

import qualified Data.List.NonEmpty     as NE
import qualified Data.Matrix            as M
import qualified Data.Set               as Set
import           Protolude
import           System.Random.SplitMix

import           Intro
import           Orphanage              ()
import           Types.Common
import           Types.Voronoi

mkVoronoiIO :: (Enum a, Bounded a) => DimX -> DimY -> Count -> IO (Voronoi a)
mkVoronoiIO dimx dimy count = do
  stdGen <- newSMGen
  let (voronoi, _) = runState (mkVoronoi dimx dimy count) stdGen
  pure voronoi

-- | Generates a voronoi diagram by randomly and safely generating enum values.
-- | The bounds are 1 indexed and therefore the upper bounds are inclusive.
mkVoronoi :: Monad m => (Enum a, Bounded a) => DimX -> DimY -> Count -> WithRandT m (Voronoi a)
mkVoronoi dimx dimy randomPoints = do
  centers <- take (fromIntegral randomPoints) <$> getRandomBoundedInf
  (_, positionedCenters) <- foldM (combineF dimx dimy) (Set.empty, []) centers
  let positionedCenters' =
        case positionedCenters of
          []     -> ((0, 0), minBound) NE.:| []
          (x:xs) -> x NE.:| xs
  let cells = M.matrix (fromIntegral dimy) (fromIntegral dimx) (\(y, x) -> findNearest x y positionedCenters')
  pure $ Voronoi cells dimx dimy positionedCenters'

findNearest :: Int -> Int -> NE.NonEmpty (PointWithInfo a) -> a
findNearest x y points =
  let points' =
        NE.sortBy (compareWith fst) $
        (\((x', y'), l) -> (abs (x - fromIntegral x') + abs (y - fromIntegral y'), l)) <$> points
   in snd $ NE.head points'

combineF :: Monad m => DimX -> DimY -> (Set Point, [PointWithInfo a]) -> a -> WithRandT m (Set Point, [PointWithInfo a])
combineF dimx dimy (set, acc) label = do
  point' <- getFreePoint dimx dimy set
  let set' = point' `Set.insert` set
  pure (set', (point', label) : acc)

getFreePoint :: Monad m => DimX -> DimY -> Set Point -> WithRandT m Point
getFreePoint dimx dimy set = do
  point' <- getRandomRange ((1, 1), (dimx, dimy))
  if point' `Set.member` set
    then getFreePoint dimx dimy set
    else pure point'
