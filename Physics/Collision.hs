module Physics.Collision where

import Physics.Vector

data Collision = Collision
  { collisionDirection :: !Vector
  , collisionNormal    :: !Vector
  } deriving Show

class Collides a where
  collides :: a -> a -> Maybe Collision
