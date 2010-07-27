module Math.Point where

import Graphics
import Math.Normalize
import Render

import Graphics.Rendering.OpenGL.GL (GLfloat)

data Point = Point
  { pointX :: !GLfloat
  , pointY :: !GLfloat
  } deriving (Eq,Show,Ord)

type Vector = Point

instance Render Point where
  render (Point x y) = vertex2d x y

instance Normalize Point where
  normalize (Point x y) = Point (inv x) (inv y)
    where
    inv 0 = 0
    inv n = 1 / n

instance Num Point where
  (*)           = pointBinary (*)
  (+)           = pointBinary (+)
  (-)           = pointBinary (-)
  abs           = pointUnary abs
  signum        = pointUnary abs
  fromInteger i = Point (fromInteger i) (fromInteger i)

pointBinary :: (GLfloat -> GLfloat -> GLfloat) -> (Point -> Point -> Point)
pointBinary f (Point x1 y1) (Point x2 y2) = Point (f x1 x2) (f y1 y2)

pointUnary :: (GLfloat -> GLfloat) -> (Point -> Point)
pointUnary f (Point x y) = Point (f x) (f y)

dot :: Point -> Point -> GLfloat
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2
{-# INLINE dot #-}

distance :: Point -> Point -> GLfloat
distance p1 p2 = sqrt (x*x + y*y)
  where
  Point x y = p1 - p2

vectorLength :: Vector -> GLfloat
vectorLength (Point x y) = sqrt (x * x + y * y)

scaleVector :: GLfloat -> Vector -> Vector
scaleVector d (Point x y) = Point (d * x) (d * y)
