module Types.Common
  ( Count
  , Dim
  , DimX
  , DimY
  , Position
  , Point
  , PointWithInfo
  , Time
  , Key
  , Keyed
  , TimeWindow
  , WithRand
  , WithRandT
  , Level(..)
  ) where

import           Protolude
import           System.Random.SplitMix

type Count = Word

type Dim = Word

type DimX = Dim

type DimY = Dim

type Position = Word

type Point = (Position, Position)

type PointWithInfo a = (Point, a)

type Time = Integer

type TimeWindow = Word

type Key = Integer

type Keyed = State Key

type WithRandT = StateT SMGen

type WithRand = State SMGen

data Level
  = None
  | VeryLow
  | Low
  | Average
  | High
  | Extreme
  | Max
  deriving (Eq, Show, Enum, Bounded)
