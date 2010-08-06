module Math.Utils (
    module Math.Utils
  , GLfloat
  ) where

import Graphics.Rendering.OpenGL.GL (GLfloat)

rangeOverlap :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> GLfloat
rangeOverlap (a1,b1) (a2,b2)
  | a2 > a1   = b1 - a2
  | otherwise = b2 - a1
