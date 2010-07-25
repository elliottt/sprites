module Math.Line where

import Math.Normalize
import Math.Point

import Graphics.Rendering.OpenGL.GL (GLfloat)

data Line = Line !Point !Point
  deriving (Eq,Show)

instance Normalize Line where
  normalize (Line p1 p2) = Line (normalize p1) (normalize p2)

lineLength :: Line -> GLfloat
lineLength (Line p1 p2) = distance p1 p2

perpendicular :: Line -> Point
perpendicular (Line (Point x1 y1) (Point x2 y2)) = normalize (p1' - p2')
  where
  p1' = Point (-y1) x1
  p2' = Point (-y2) x2
