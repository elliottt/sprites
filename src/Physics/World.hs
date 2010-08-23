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

import Debug.Trace


type Body = PhysicalState Shape

data World = World
  { worldBox         :: !AABB
  , worldBodies      :: [PhysicalState Shape]
  , worldGravity     :: Maybe (Vector GLfloat)
  }

instance Render World where
  render w = render (worldBodies w)

emptyWorld :: GLfloat -> GLfloat -> World
emptyWorld w h = World
  { worldBox         = AABB (Point (-w / 2) (h / 2)) w h
  , worldBodies      = []
  , worldGravity     = Nothing
  }

stepWorld :: Interval -> World -> World
stepWorld dt0 w = w
  { worldBodies = ds' ++ ss
  }
  where
  dt          = fromIntegral dt0 / 1000
  (ds,ss)     = collisions w
  ds'         = mapMaybe step ds
  step (p,cs) = do
    let resolve (c,q) = resolveCollision w c p q
    case map resolve cs of

      [] -> do
        let p' = stepPhysicalState dt p
        guard (psAABB p' `aabbOverlap` worldBox w)
        return p'

      vs -> do
        let (i,v') = foldl1 (\(x,y) (a,b) -> (x +^ a, y +^ b)) vs
        let p'     = stepPhysicalState dt
                   $ applyImpulse v'
                   $ moveBy i p
        guard (psAABB p' `aabbOverlap` worldBox w)
        return p'

-- | Turn a collision into a displacement vector, and a new velocity.
resolveCollision :: World -> Collision -> Body -> Body
                 -> (Vector GLfloat,Vector GLfloat)
resolveCollision w c p q = debug (disp,v')
  where
  disp  = collisionOverlap c *^ n
  n     = collisionNormal c
  n'    = normalV n
  v     = psVelocity p
  nv    = n <.> v
  nperp = projAlong v n'
  rest  = psRestitution p
  v'    = nperp -^ (rest * nv *^ n)

  debug r | psDebug p = show c `trace` r
          | otherwise = r

collisions :: World -> ([(Body,[(Collision,Body)])], [Body])
collisions w = loop ds [] []
  where
  (ss,ds) = partition isStationary (worldBodies w)

  -- collect collisions
  loop []     _  rs = (rs,ss)
  loop (a:as) bs rs = loop as (a:bs) (r:rs)
    where
    r      = (a, mapMaybe step (as ++ bs ++ ss))
    step s = do
      c <- collides a s
      return (c,s)

addBody :: Body -> World -> World
addBody b w = w { worldBodies = b' : worldBodies w }
  where
  b' | psStatic b = b
     | otherwise  = b { psAcceleration = (+^) (psAcceleration b)
                                       $ fromMaybe zero
                                       $ worldGravity w }
