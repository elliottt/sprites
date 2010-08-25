module Physics.World where

import Graphics
import Math.AffinePlane
import Math.Utils
import Physics.AABB
import Physics.Body
import Physics.Shape
import Time

import Control.Monad (guard)
import Data.List (partition)
import Data.Maybe (fromMaybe,mapMaybe)
import qualified Data.Set as Set

import Debug.Trace


type Body = PhysicalState

data World = World
  { worldBox         :: !AABB
  , worldBodies      :: [Body]
  , worldGravity     :: Maybe (Vector GLfloat)
  }

instance Render World where
  render w = render (worldBodies w)

emptyWorld :: GLfloat -> GLfloat -> World
emptyWorld w h = World
  { worldBox         = AABB (Point 0 h) w h
  , worldBodies      = []
  , worldGravity     = Nothing
  }

-- | Move the world forward
stepWorld :: Interval -> World -> IO World
stepWorld dt0 w = do
  let dt = fromIntegral dt0 / 1000
  handleCollisions dt w

handleCollisions :: GLfloat -> World -> IO World
handleCollisions dt w = do
  (ds,ss) <- collisions w
  let step (b1,b2,cs) = do
        let resolve c = resolveCollision w c b1 b2
        case map resolve (Set.toList cs) of

          [] -> stepPhysicalState dt b1

          -- calculate the impulse force, and apply it
          vs -> stepPhysicalState dt b1

  ds' <- mapM step ds
  return w { worldBodies = ss ++ ds' }

-- | Turn a collision into a new velocity.
resolveCollision :: World -> Contact -> Body -> Body -> Vector GLfloat
resolveCollision w c b1 b2 = error "resolveCollision"

-- | Partition out collisions, and static bodies.
collisions :: World -> IO ([(Body,Body,Set.Set Contact)], [Body])
collisions w = error "collisions"

addBody :: Body -> World -> World
addBody b w = w { worldBodies = b' : worldBodies w }
  where
  b' | psStatic b = b
     | otherwise  = b { psAcceleration = (+^) (psAcceleration b)
                                       $ fromMaybe zero
                                       $ worldGravity w }
