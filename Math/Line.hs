module Math.Line where

import Math.Normalize
import Math.Point
import Render

import Graphics.Rendering.OpenGL.GL (GLfloat)

data Line = Line !Point !Point
  deriving (Eq,Show)

instance Render Line where
  render (Line p1 p2) = render p1 >> render p2

instance Normalize Line where
  normalize (Line p1 p2) = Line (normalize p1) (normalize p2)

lineLength :: Line -> GLfloat
lineLength (Line p1 p2) = distance p1 p2

perpendicular :: Line -> Point
perpendicular (Line p1 p2) = normal (p1 - p2)
