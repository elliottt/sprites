module Physics.Body (
    Shape
  , circle
  , polygon

  , PhysicalState(..)
  , Body
  ) where

import Math.Point

import Graphics.Rendering.OpenGL.GL (GLfloat)

data PhysicalState a = PhysicalState
  { psVelocity :: !Vector
  } deriving Show

type Body = PhysicalState Shape

