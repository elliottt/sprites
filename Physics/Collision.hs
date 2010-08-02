module Physics.Collision where

import Math.Utils
import Physics.Vector

data Collision = Collision
  { collisionDirection :: !Vector
  , collisionOverlap   :: !GLfloat
  , collisionNormal    :: !Vector
  } deriving Show

class Collides a where
  collides :: a -> a -> Maybe Collision
