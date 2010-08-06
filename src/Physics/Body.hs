module Physics.Body (
    PhysicalState(..)
  , staticBody, dynamicBody
  , Physical(..)
  , psVelocity

  , moveBy
  , isStationary
  , stepPhysicalState
  , applyImpulse
  , setDebug
  , setRestitution
  ) where

import Graphics
import Math.AffinePlane
import Math.Utils
import Physics.AABB
import Physics.Collision

import Control.Monad (guard,when)


data PhysicalState a = PhysicalState
  { psTransform    :: !Matrix
  , psAcceleration :: !Vector
  , psMass         :: !GLfloat
  , psFriction     :: !GLfloat
  , psAABB         :: !AABB
  , psRestitution  :: !GLfloat
  , psStatic       :: Bool
  , psResting      :: Bool
  , psDebug        :: Bool
  , psData         :: a
  } deriving Show

instance Render a => Render (PhysicalState a) where
  render ps = do
    --when (psDebug ps) (render (psAABB ps))
    render (psData ps)

instance Collides a => Collides (PhysicalState a) where
  collides a b = do
    guard (aabbOverlap (psAABB a) (psAABB b))
    collides (psData a) (psData b)

class Physical a where
  boundingBox :: a -> IO AABB
  transform   :: Matrix -> a -> IO ()
  position    :: a -> IO Point

psVelocity :: PhysicalState a -> Vector
psVelocity ps = Vector (mat02 t) (mat12 t)
  where
  t = psTransform ps

moveBy :: Physical a => Vector -> PhysicalState a -> IO (PhysicalState a)
moveBy (Vector x y) ps = do
  transform (1 { mat02 = x, mat12 = y }) (psData ps)
  bb' <- boundingBox (psData ps)
  return ps { psAABB = bb' }

dynamicBody :: Physical a => a -> IO (PhysicalState a)
dynamicBody a = do
  aabb <- boundingBox a
  return PhysicalState
    { psTransform    = 1
    , psAcceleration = Vector 0 0
    , psMass         = 0
    , psFriction     = 0
    , psAABB         = aabb
    , psStatic       = False
    , psResting      = False
    , psDebug        = False
    , psRestitution  = 1
    , psData         = a
    }

staticBody :: Physical a => a -> IO (PhysicalState a)
staticBody a = do
  aabb <- boundingBox a
  return PhysicalState
    { psTransform    = 1
    , psAcceleration = Vector 0 0
    , psMass         = 0
    , psFriction     = 0
    , psAABB         = aabb
    , psStatic       = False
    , psResting      = False
    , psDebug        = False
    , psRestitution  = 1
    , psData         = a
    }

isStationary :: PhysicalState a -> Bool
isStationary ps = or
  [ psStatic ps
  , psResting ps
  , isZero (psAcceleration ps) && isZero (psTransform ps)
  ]

stepPhysicalState :: Physical a
                  => GLfloat -> PhysicalState a -> IO (PhysicalState a)
stepPhysicalState dt ps
  | isStationary ps = return ps
  | otherwise       = do
    -- modify the translation vector of the transformation matrix
    let t            = psTransform ps
        Vector dx dy = scaleVector dt (psAcceleration ps)
        t'           = t { mat02 = mat02 t + dx, mat12 = mat12 t + dy }
    transform t (psData ps)
    aabb' <- boundingBox (psData ps)
    return ps
      { psAABB      = aabb'
      , psTransform = t'
      }

applyImpulse :: Vector -> PhysicalState a -> PhysicalState a
applyImpulse (Vector x y) ps = ps
  { psTransform = t { mat02 = x, mat12 = y }
  }
  where
  t = psTransform ps

setDebug :: Bool -> PhysicalState a -> PhysicalState a
setDebug b ps = ps { psDebug = b }

setRestitution :: GLfloat -> PhysicalState a -> PhysicalState a
setRestitution r ps = ps { psRestitution = r }
