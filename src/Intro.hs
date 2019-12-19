module Intro
  ( getRandom
  , getRandomInf
  , getRandomBounded
  , toWrappedEnum
  ) where

import           ClassyPrelude
import           Control.Monad.Trans.State.Lazy
import           System.Random

getRandomBounded :: (Monad m, Enum a, Bounded a) => StateT StdGen m a
getRandomBounded = do
  word :: Word <- getRandom
  return $ toWrappedEnum word

-- | Get a random thing. Update the state.
getRandom :: (Monad m, Random a) => StateT StdGen m a
getRandom = do
  (a, g) <- random <$> get
  put g
  return a

-- | Get an infinite list of random things. Split the generator before, update accordingly.
getRandomInf :: (Monad m, Random a) => StateT StdGen m [a]
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

