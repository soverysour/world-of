module Main
  ( main
  ) where

import           Protolude
import           System.Random.SplitMix

import           World
import           World.Config

main :: IO ()
main = do
  world <- createWorld testWorld
  gen <- newSMGen
  print world
  print . flip runStateT 0 . flip runStateT gen $ tickWorld 0 defaultWindowSize world
