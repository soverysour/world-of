{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orphanage
  ( toWrappedEnum
  ) where

import           ClassyPrelude
import           System.Random

-- | Safe conversion from a Word to a bounded enum type. Wraps around its max bound.
toWrappedEnum ::
     forall a. (Enum a, Bounded a)
  => Word
  -> a
toWrappedEnum w = toEnum $ fromIntegral v
  where
    maxB = maxBound :: a
    boundMaxB = 1 + fromIntegral (fromEnum maxB)
    v = w `mod` boundMaxB

-- | Random instance for tuple2.
instance (Random a, Random b) => Random (a, b) where
  random g =
    let (a, g1) = random g
        (b, g2) = random g1
     in ((a, b), g2)
  randomR ((la, lb), (ha, hb)) g =
    let (a, g1) = randomR (la, ha) g
        (b, g2) = randomR (lb, hb) g1
     in ((a, b), g2)
-- TODO: This overlaps with the `Word` instance. Figure out a way around this.
-- TODO: Also, it requires UndecidableInstances.
-- instance (Enum a, Bounded a) => Random a where
--   random g =
--     let (w, g1) = random g
--      in (toWrappedEnum w, g1)
--   randomR (la, ha) g =
--     let (w, g1) =
--           randomR (fromIntegral $ fromEnum la, fromIntegral $ fromEnum ha) g
--      in (toWrappedEnum w, g1)
