module Config
  ( ServerConfig(..)
  , testServer
  , WorldConfig(..)
  , testWorld
  , smallWorld
  , largeWorld
  ) where

import           Prelude      (String)
import           Protolude

import           Types.Common

data ServerConfig =
  ServerConfig
    { host :: String
    , port :: Int
    }
  deriving (Eq, Show)

testServer :: ServerConfig
testServer = ServerConfig "127.0.0.1" 8000

data WorldConfig =
  WorldConfig
    { sizeX      :: Dim
    , sizeY      :: Dim
    , zones      :: Dim
    , windowSize :: TimeWindow
    , startTime  :: Time
    , startKey   :: Key
    , startSteps :: Count
    , tickPeriod :: TimeWindow
    }
  deriving (Eq, Show)

testWorld :: WorldConfig
testWorld = withDefaults 3 3 2

smallWorld :: WorldConfig
smallWorld = withDefaults 15 15 6

largeWorld :: WorldConfig
largeWorld = withDefaults 40 40 15

withDefaults :: Dim -> Dim -> Count -> WorldConfig
withDefaults dimx dimy zoneCount =
  WorldConfig
    { sizeX = dimx
    , sizeY = dimy
    , zones = zoneCount
    , windowSize = 5
    , startTime = 0
    , startKey = 1
    , startSteps = 150
    , tickPeriod = 2
    }
