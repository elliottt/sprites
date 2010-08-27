module Physics.Body (
    PhysicalState(..)
  , staticBody, dynamicBody
  , psVelocity

  , stepPhysicalState
  , adjustVelocity
  , collidePhysicalState

  , moveBy
  , isStationary
  , applyImpulse
  , setDebug
  , setRestitution
  ) where

import Graphics
import Math.AffinePlane
import Math.Utils
import Physics.AABB
import Physics.Shape

import Control.Monad (guard,when)
import qualified Data.Set as Set


data PhysicalState = PhysicalState
  { psTransform    :: !(Matrix GLfloat)
  , psAcceleration :: !(Vector GLfloat)
  , psMass         :: !GLfloat
  , psFriction     :: !GLfloat
  , psAABB         :: !AABB
  , psRestitution  :: !GLfloat
  , psStatic       :: Bool
  , psResting      :: Bool
  , psDebug        :: Bool
  , psShape        :: Shape
  , psId           :: !Int
  }

instance Show PhysicalState where
  show ps = "<body:" ++ show (psId ps) ++ ">"

instance Render PhysicalState where
  render ps = do
    render (psShape ps)
    when (psDebug ps) (render (psAABB ps))

psVelocity :: PhysicalState -> (Vector GLfloat)
psVelocity ps = Vector (mat02 t) (mat12 t)
  where
  t = psTransform ps

moveBy :: Vector GLfloat -> PhysicalState -> IO PhysicalState
moveBy (Vector x y) ps = do
  transformShape (idM { mat02 = x, mat12 = y }) (psShape ps)
  bb' <- shapeAABB (psShape ps)
  return ps { psAABB = bb' }

dynamicBody :: Shape -> IO PhysicalState
dynamicBody a = do
  aabb <- shapeAABB a
  return PhysicalState
    { psTransform    = idM
    , psAcceleration = Vector 0 0
    , psMass         = 0
    , psFriction     = 0
    , psAABB         = aabb
    , psStatic       = False
    , psResting      = False
    , psDebug        = False
    , psRestitution  = 1
    , psShape        = a
    , psId           = 0
    }

staticBody :: Shape -> IO PhysicalState
staticBody a = do
  aabb <- shapeAABB a
  return PhysicalState
    { psTransform    = idM
    , psAcceleration = Vector 0 0
    , psMass         = 0
    , psFriction     = 0
    , psAABB         = aabb
    , psStatic       = False
    , psResting      = False
    , psDebug        = False
    , psRestitution  = 1
    , psShape        = a
    , psId           = 0
    }

isStationary :: PhysicalState -> Bool
isStationary ps = or
  [ psStatic ps
  , psResting ps
  , isZero (psAcceleration ps) && isZero (psTransform ps)
  ]

-- | Move the body by the current velocity.
stepPhysicalState :: GLfloat -> PhysicalState -> IO PhysicalState
stepPhysicalState dt ps
  | isStationary ps = return ps
  | otherwise       = do
    transformShape (psTransform ps) (psShape ps)
    aabb' <- shapeAABB (psShape ps)
    return ps { psAABB = aabb' }

-- | Modify the translation vector of the transformation matrix
adjustVelocity :: GLfloat -> PhysicalState -> PhysicalState
adjustVelocity dt ps
  | isStationary ps = ps
  | otherwise       = ps { psTransform = t' }
    where
    t            = psTransform ps
    Vector dx dy = dt *^ psAcceleration ps
    t'           = t { mat02 = mat02 t + dx, mat12 = mat12 t + dy }

collidePhysicalState :: PhysicalState -> PhysicalState
                     -> IO (Maybe (PhysicalState,PhysicalState,Set.Set Contact))
collidePhysicalState ps1 ps2 =
  fmap mk `fmap` shapeCollision (psShape ps1) (psShape ps2)
  where
  mk cs = (ps1,ps2,cs)

applyImpulse :: Vector GLfloat -> PhysicalState -> PhysicalState
applyImpulse (Vector x y) ps = ps
  { psTransform = t { mat02 = x, mat12 = y }
  }
  where
  t = psTransform ps

setTransform :: Matrix GLfloat -> PhysicalState -> PhysicalState
setTransform mat ps = ps { psTransform = mat }

addTransform :: Matrix GLfloat -> PhysicalState -> PhysicalState
addTransform mat ps = ps { psTransform = addMatrix (psTransform ps) mat }

setDebug :: Bool -> PhysicalState -> PhysicalState
setDebug b ps = ps { psDebug = b }

setRestitution :: GLfloat -> PhysicalState -> PhysicalState
setRestitution r ps = ps { psRestitution = r }

-- | Given two bodies, calculate the resulting velocity on the first given the
-- contact set.

