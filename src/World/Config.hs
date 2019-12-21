module World.Config
  ( WorldConfig(..)
  , testWorld
  , smallWorld
  , largeWorld
  , defaultWindowSize
  ) where

import           Types.Common

data WorldConfig =
  WorldConfig
    { sizeX      :: Dim
    , sizeY      :: Dim
    , zones      :: Dim
    , windowSize :: TimeWindow
    }
    
testWorld :: WorldConfig
testWorld = WorldConfig 3 3 2 defaultWindowSize

smallWorld :: WorldConfig
smallWorld = WorldConfig 15 15 6 defaultWindowSize

largeWorld :: WorldConfig
largeWorld = WorldConfig 40 40 15 defaultWindowSize

defaultWindowSize :: TimeWindow
defaultWindowSize = 5
