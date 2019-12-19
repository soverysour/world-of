module Main
  ( main
  ) where

import           ClassyPrelude
import           Text.Pretty.Simple (pPrint)

import           World
import           World.Config

main :: IO ()
main = do
  world <- createWorld smallWorld
  pPrint world
