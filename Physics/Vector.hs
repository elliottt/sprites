module Physics.Vector where

import Math.Normalize
import Math.Point (Point(..))

import Graphics.Rendering.OpenGL.GL (GLfloat)

data Vector = Vector !GLfloat !GLfloat
  deriving (Eq,Show,Ord)

instance Normalize Vector where
  normalize v@(Vector x y) = Vector (x / len) (y / len)
    where len = vectorLength v

mapVector :: (GLfloat -> GLfloat) -> Vector -> Vector
mapVector f (Vector x y) = Vector (f x) (f y)

dotProduct :: Vector -> Vector -> GLfloat
dotProduct (Vector x1 y1) (Vector x2 y2) = x1*x2 + y1*y2

vectorLength :: Vector -> GLfloat
vectorLength (Vector x y) = sqrt (x*x + y*y)

invertVector :: Vector -> Vector
invertVector (Vector x y) = Vector (-x) (-y)

-- | Turn a point into a vector.
pointToVector :: Point -> Vector
pointToVector (Point x y) = Vector x y

scaleVector :: GLfloat -> Vector -> Vector
scaleVector d (Vector x y) = Vector (d*x) (d*y)
