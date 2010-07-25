module Math.Point where

import Math.Normalize

import Graphics.Rendering.OpenGL.GL (GLfloat)

data Point = Point
  { pointX :: !GLfloat
  , pointY :: !GLfloat
  } deriving (Eq,Show)

instance Normalize Point where
  normalize (Point x y) = Point (1 / x) (1 / y)

dot :: Point -> Point -> GLfloat
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2
{-# INLINE dot #-}

distance :: Point -> Point -> GLfloat
distance (Point x1 y1) (Point x2 y2) = sqrt (x1*x2 + y1*y2)
