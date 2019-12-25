module Generator.Areas
  ( mkMapAreaIO
  , mkMapArea
  ) where

import           Protolude
import           System.Random.SplitMix

import           Intro
import           Types.Common
import           Types.World
import qualified Types.World.Area.Desert   as D
import qualified Types.World.Area.Mountain as M
import qualified Types.World.Area.Normal   as N
import qualified Types.World.Area.Rocky    as R
import qualified Types.World.Area.Swamp    as S
import qualified Types.World.Area.Tundra   as T
import qualified Types.World.Area.Water    as W

mkMapAreaIO :: WorldZone -> IO WorldArea
mkMapAreaIO zone = do
  stdGen <- newSMGen
  let (area, _) = runState (mkMapArea zone) stdGen
  pure area

mkMapArea :: Monad m => WorldZone -> WithRandT m WorldArea
mkMapArea Water    = WorldArea <$> randomWater
mkMapArea Mountain = WorldArea <$> randomMountain
mkMapArea Normal   = WorldArea <$> randomNormal
mkMapArea Tundra   = WorldArea <$> randomTundra
mkMapArea Desert   = WorldArea <$> randomDesert
mkMapArea Swamp    = WorldArea <$> randomSwamp
mkMapArea Rocky    = WorldArea <$> randomRocky

randomRocky :: Monad m => WithRandT m R.Rocky
randomRocky = do
  animalPopulation <- getRandomBounded
  R.Rocky animalPopulation <$> getRandomBounded

randomSwamp :: Monad m => WithRandT m S.Swamp
randomSwamp = do
  animalPopulation <- getRandomBounded
  depth <- getRandomBounded
  S.Swamp animalPopulation depth <$> getRandomBounded

randomDesert :: Monad m => WithRandT m D.Desert
randomDesert = do
  elevation <- getRandomBounded
  animalPopulation <- getRandomBounded
  D.Desert elevation animalPopulation <$> getRandomBounded

randomTundra :: Monad m => WithRandT m T.Tundra
randomTundra = do
  elevation <- getRandomBounded
  animalPopulation <- getRandomBounded
  forestation <- getRandomBounded
  T.Tundra elevation animalPopulation forestation <$> getRandomBounded

randomNormal :: Monad m => WithRandT m N.Normal
randomNormal = do
  elevation <- getRandomBounded
  animalPopulation <- getRandomBounded
  forestation <- getRandomBounded
  N.Normal elevation animalPopulation forestation <$> getRandomBounded

randomMountain :: Monad m => WithRandT m M.Mountain
randomMountain = do
  elevation <- getRandomBounded
  animalPopulation <- getRandomBounded
  forestation <- getRandomBounded
  M.Mountain elevation animalPopulation forestation <$> getRandomBounded

randomWater :: Monad m => WithRandT m W.Water
randomWater = do
  isSalty <- getRandomBounded
  animalPopulation <- getRandomBounded
  depth <- getRandomBounded
  W.Water isSalty animalPopulation depth <$> getRandomBounded
