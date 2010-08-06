module Physics.Collision where

import Math.AffinePlane
import Math.Utils

class Collides a where
  collides   :: a -> a -> Maybe Collision
  --contactSet :: a -> a -> Maybe ContactSet

data Collision = Collision
  { collisionDirection :: !(Vector GLfloat)
  , collisionOverlap   :: !GLfloat
  , collisionNormal    :: !(Vector GLfloat)
  , collisionProjInfo  :: !ProjInfo
  } deriving Show

-- | Projection information.
data ProjInfo = ProjInfo
  { projMin       :: !Int
  , projMinUnique :: Bool
  , projMax       :: !Int
  , projMaxUnique :: Bool
  } deriving Show
