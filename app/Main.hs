{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import           ClassyPrelude
import           Text.Pretty.Simple (pPrint)

import           Voronoi

data Labels
  = Sea
  | Mountain
  | Land
  | Tundra
  deriving (Eq, Show, Enum, Bounded)

main :: IO ()
main = do
  map' :: Voronoi Labels <- mkVoronoiIO 4 2 3
  pPrint map'
