module Physics.Body (
    PhysicalState(..)
  , mkPhysicalState
  , Physical(..)

  , moveBy
  , isStationary
  , stepPhysicalState
  , applyImpulse
  ) where

import Graphics
import Math.Matrix
import Math.Point
import Math.Utils
import Physics.AABB
import Physics.Collision
import Physics.Vector

import Control.Monad (guard)


data PhysicalState a = PhysicalState
  { psTransform    :: !Matrix
  , psAcceleration :: !Vector
  , psMass         :: !GLfloat
  , psFriction     :: !GLfloat
  , psAABB         :: !AABB
  , psStatic       :: Bool
  , psData         :: a
  } deriving Show

instance Render a => Render (PhysicalState a) where
  render ps = render (psData ps)

instance Collides a => Collides (PhysicalState a) where
  collides a b = do
    guard (aabbOverlap (psAABB a) (psAABB b))
    collides (psData a) (psData b)

class Physical a where
  boundingBox :: a -> AABB
  transform   :: Matrix -> a -> a
  position    :: a -> Point

moveBy :: Physical a => Vector -> PhysicalState a -> PhysicalState a
moveBy (Vector x y) ps = ps
  { psData = a'
  , psAABB = boundingBox a'
  }
  where
  a' = transform (0 { mat02 = x, mat12 = y }) (psData ps)

mkPhysicalState :: Physical a => a -> PhysicalState a
mkPhysicalState a = PhysicalState
  { psTransform    = 1
  , psAcceleration = Vector 0 0
  , psMass         = 0
  , psFriction     = 0
  , psAABB         = boundingBox a
  , psStatic       = False
  , psData         = a
  }

isStationary :: PhysicalState a -> Bool
isStationary ps =
  psStatic ps || (isZero (psAcceleration ps) && isZero (psTransform ps))

stepPhysicalState :: Physical a
                  => GLfloat -> PhysicalState a -> PhysicalState a
stepPhysicalState dt ps = ps
  { psData      = a'
  , psAABB      = aabb'
  , psTransform = t'
  }
  where
  -- modify the translation vector of the transformation matrix
  t            = psTransform ps
  Vector dx dy = scaleVector dt (psAcceleration ps)
  t'           = t { mat02 = mat02 t + dx, mat12 = mat12 t + dy }
  a'           = transform t' (psData ps)
  aabb'        = boundingBox a'

applyImpulse :: Vector -> PhysicalState a -> PhysicalState a
applyImpulse (Vector x y) ps = ps
  { psTransform = t { mat02 = mat02 t + x, mat12 = mat12 t + y }
  }
  where
  t = psTransform ps
