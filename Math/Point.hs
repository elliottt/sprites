module Math.Point where

import Graphics.Rendering.OpenGL.GL (GLfloat)

data Point = Point
  { pointX :: !GLfloat
  , pointY :: !GLfloat
  } deriving (Eq,Show)

dot :: Point -> Point -> GLfloat
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2
{-# INLINE dot #-}
