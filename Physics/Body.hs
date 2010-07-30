module Physics.Body (
    PhysicalState(..)
  , mkPhysicalState
  , Physical(..)

  , isStationary
  , stepPhysicalState
  ) where

import Math.Point
import Math.Utils
import Physics.AABB
import Physics.Collision
import Physics.Vector

import Control.Monad (guard)


data PhysicalState a = PhysicalState
  { psVelocity     :: !Vector
  , psAcceleration :: !Vector
  , psRotation     :: !GLfloat
  , psMass         :: !GLfloat
  , psFriction     :: !GLfloat
  , psAABB         :: !AABB
  , psData         :: a
  } deriving Show

instance Collides a => Collides (PhysicalState a) where
  collides a b = do
    guard (aabbOverlap (psAABB a) (psAABB b))
    collides (psData a) (psData b)

class Physical a where
  boundingBox :: a -> AABB
  moveBy      :: Vector -> a -> a
  position    :: a -> Point

mkPhysicalState :: Physical a => a -> PhysicalState a
mkPhysicalState a = PhysicalState
  { psVelocity     = Vector 0 0
  , psAcceleration = Vector 0 0
  , psRotation     = 0
  , psMass         = 0
  , psFriction     = 0
  , psAABB         = boundingBox a
  , psData         = a
  }

isStationary :: PhysicalState a -> Bool
isStationary ps = isZero (psVelocity ps) || isZero (psAcceleration ps)

stepPhysicalState :: Physical a
                  => GLfloat -> PhysicalState a -> PhysicalState a
stepPhysicalState dt ps = ps
  { psData     = a'
  , psAABB     = aabb'
  , psVelocity = v'
  }
  where
  v     = psVelocity ps
  a'    = moveBy v (psData ps)
  aabb' = boundingBox a'
  -- newton's method!
  v'    = v `addVector` scaleVector dt (psAcceleration ps)
