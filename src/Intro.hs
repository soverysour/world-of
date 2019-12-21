{-# LANGUAGE ScopedTypeVariables #-}

module Intro
  ( getRandom
  , getRandomInf
  , getRandomBounded
  , getRandomBoundedInf
  , getRandomRange
  , toWrappedEnum
  , compareWith
  ) where

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
  return a

-- | Get an infinite list of random things. Split the generator before, update accordingly.
getRandomInf :: (Monad m, Random a) => StateT SMGen m [a]
getRandomInf = do
  (g1, g2) <- split <$> get
  put g2
  return $ randoms g1

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

getRandomRange :: (Monad m, Random a) => (a, a) -> StateT SMGen m a
getRandomRange bounds = do
  (a, g) <- randomR bounds <$> get
  put g
  return a
