module Physics.Collision where

import Physics.Vector

data Collision = Collision
  { collisionDirection :: !Vector
  }

class Collides a where
  collides :: a -> a -> Maybe Collision
