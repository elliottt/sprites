module Math.Line where

import Math.Point

data Line = Line !Point !Point
  deriving (Eq,Show)
