module Types.Common
  ( Count
  , Dim
  , DimX
  , DimY
  , Label
  , Pos
  , Point
  , PointWithInfo
  , LabeledPoint
  , Level(..)
  ) where

import           ClassyPrelude

type Count = Word

type Dim = Word

type DimX = Dim

type DimY = Dim

type Label = Word

type Pos = Word

type Point = (Pos, Pos)

type PointWithInfo a = (Point, a)

type LabeledPoint = PointWithInfo Label

data Level
  = None
  | VeryLow
  | Low
  | Average
  | High
  | Extreme
  | Max
  deriving (Eq, Show, Enum, Bounded)
