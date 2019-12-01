module Voronoi
  ( Voronoi(..)
  , mkVoronoiIO
  , mkVoronoi
  ) where

import           ClassyPrelude
import           Control.Monad.Trans.State.Lazy
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as Set
import qualified Data.Vector                    as V
import           System.Random

import           Orphanage

data Voronoi a =
  Voronoi
    { cells :: Vector (Vector a)
    , dimx  :: DimX
    , dimy  :: DimY
    }
  deriving (Eq, Show)

mkVoronoiIO :: (Enum a, Bounded a) => DimX -> DimY -> Count -> IO (Voronoi a)
mkVoronoiIO dimx dimy count = do
  let voronoiStateT = mkVoronoi' dimx dimy count
  stdGen <- getStdGen
  let (voronoi, g) = runState voronoiStateT stdGen
  setStdGen g
  return voronoi

mkVoronoi ::
     (Enum a, Bounded a)
  => DimX
  -> DimY
  -> Count
  -> StdGen
  -> (Voronoi a, StdGen)
mkVoronoi dimx dimy count gen =
  let voronoiStateT = mkVoronoi' dimx dimy count
   in runState voronoiStateT gen

-- | Generates a voronoi diagram by randomly and safely generating enum values.
-- | The bounds are 0 indexed and therefore the upper bounds are exclusive.
mkVoronoi' ::
     (Enum a, Bounded a) => DimX -> DimY -> Count -> State StdGen (Voronoi a)
mkVoronoi' dimx dimy randomPoints = do
  centers <- take (fromIntegral randomPoints) <$> getRandomInf
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
  -> State StdGen (Set Point, [LabeledPoint])
combineF dimx dimy (set, acc) label = do
  point' <- getFreePoint dimx dimy set
  let set' = point' `Set.insert` set
  return (set', (point', label) : acc)

getFreePoint :: DimX -> DimY -> Set Point -> State StdGen Point
getFreePoint dimx dimy set = do
  (x, y) <- getRandom
  let point' = (x `mod` dimx, y `mod` dimy)
  if point' `Set.member` set
    then getFreePoint dimx dimy set
    else return point'

-- | Get a random number. Update the state.
getRandom :: (Monad m, Random a) => StateT StdGen m a
getRandom = do
  (a, g) <- random <$> get
  put g
  return a

-- | Get an infinite list of random numbers. Split the generator before, update accordingly.
getRandomInf :: (Monad m, Random a) => StateT StdGen m [a]
getRandomInf = do
  (g1, g2) <- split <$> get
  put g2
  return $ randoms g1

type Count = Word

type Dim = Word

type DimX = Dim

type DimY = Dim

type Label = Word

type Pos = Word

type Point = (Pos, Pos)

type PointWithInfo a = (Point, a)

type LabeledPoint = PointWithInfo Label
