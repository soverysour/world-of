{-# LANGUAGE ScopedTypeVariables #-}

module Intro
  ( getRandom
  , getRandomInf
  , getRandomBounded
  , getRandomBoundedInf
  , getRandomRange
  , toWrappedEnum
  , compareWith
  , foreachM
  , runEffect'
  , chanToPipeProd
  , pipeProdToChan
  , collectLeft
  , collectRight
  ) where

import           Pipes
import           Protolude
import           System.Random
import           System.Random.SplitMix

getRandomBounded :: (Monad m, Enum a, Bounded a) => StateT SMGen m a
getRandomBounded = toWrappedEnum <$> getRandom

getRandomBoundedInf :: (Monad m, Enum a, Bounded a) => StateT SMGen m [a]
getRandomBoundedInf = fmap toWrappedEnum <$> getRandomInf

-- | Get a random thing. Update the state.
getRandom :: (Monad m, Random a) => StateT SMGen m a
getRandom = do
  (a, g) <- random <$> get
  put g
  pure a

-- | Get an infinite list of random things. Split the generator before, update accordingly.
getRandomInf :: (Monad m, Random a) => StateT SMGen m [a]
getRandomInf = do
  (g1, g2) <- split <$> get
  put g2
  pure $ randoms g1

getRandomRange :: (Monad m, Random a) => (a, a) -> StateT SMGen m a
getRandomRange bounds = do
  (a, g) <- randomR bounds <$> get
  put g
  pure a

-- | Safe conversion from a Word to a bounded enum type. Wraps around its max bound.
-- | Takes into account negative lower bound.
toWrappedEnum ::
     forall a. (Enum a, Bounded a)
  => Word
  -> a
toWrappedEnum w = toEnum $ fromIntegral v
  where
    maxB = maxBound :: a
    minB = minBound :: a
    boundMinB = fromIntegral $ fromEnum minB
    boundMaxB = 1 + fromIntegral (fromEnum maxB) + boundMinB
    v = (w `mod` boundMaxB) - boundMinB

compareWith :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareWith f a b = compare (f a) (f b)

foreachM :: (Num n, Enum n, Monad m) => (b -> m b) -> b -> n -> m b
foreachM f init count = foldM (\b _ -> f b) init [1 .. count]

runEffect' :: Effect IO r -> IO ()
runEffect' = void . forkIO . void . runEffect

chanToPipeProd :: Chan a -> Producer a IO ()
chanToPipeProd chan = do
  element <- lift $ readChan chan
  yield element
  chanToPipeProd chan

pipeProdToChan :: Producer a IO () -> IO (Chan a)
pipeProdToChan prod = do
  chan <- newChan
  let consumer = do
        element <- await
        lift $ writeChan chan element
        consumer
  runEffect' $ prod >-> consumer
  pure chan

collectLeft :: Monad m => Pipe (Either a b) a m r
collectLeft = do
  element <- await
  case element of
    Left e -> do
      yield e
      collectLeft
    Right _ -> collectLeft

collectRight :: Monad m => Pipe (Either a b) b m r
collectRight = do
  element <- await
  case element of
    Right e -> do
      yield e
      collectRight
    Left _ -> collectRight
