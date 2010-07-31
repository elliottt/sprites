module Physics.World where

import Graphics
import Math.Point
import Physics.AABB
import Physics.Body
import Physics.Collision
import Physics.Shape
import Physics.Vector
import Time

import Control.Monad (guard)
import Data.List (partition)
import Data.Maybe (fromMaybe,mapMaybe)


type Body = PhysicalState Shape

data World = World
  { worldBox     :: !AABB
  , worldBodies  :: [PhysicalState Shape]
  , worldGravity :: Maybe Vector
  }

instance Render World where
  render w = render (worldBodies w)

emptyWorld w h = World
  { worldBox     = AABB (Point (-w / 2) (h / 2)) (Point w h)
  , worldBodies  = []
  , worldGravity = Nothing
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
    let p' = stepPhysicalState dt p
    guard (psAABB p' `aabbOverlap` worldBox w)
    return p'

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
     | otherwise  = b { psAcceleration = addVector (psAcceleration b)
                                       $ fromMaybe zeroVector
                                       $ worldGravity w }


