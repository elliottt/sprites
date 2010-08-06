module Physics.Collision where

import Math.Point
import Math.Utils
import Physics.Vector

class Collides a where
  collides   :: a -> a -> Maybe Collision
  --contactSet :: a -> a -> Maybe ContactSet

data Collision = Collision
  { collisionDirection :: !Vector
  , collisionOverlap   :: !GLfloat
  , collisionNormal    :: !Vector
  , collisionProjInfo  :: !ProjInfo
  } deriving Show

-- | Projection information.
data ProjInfo = ProjInfo
  { projMin       :: !Int
  , projMinUnique :: Bool
  , projMax       :: !Int
  , projMaxUnique :: Bool
  } deriving Show
