module Physics.World where

import Physics.AABB
import Physics.Body
import Physics.Shape
import Time

import Data.List (partition)


data World = World
  { worldBox    :: !AABB
  , worldBodies :: [PhysicalState Shape]
  }

stepWorld :: Interval -> World -> World
stepWorld dt0 w = w
  { worldBodies = ds ++ ss
  }
  where
  dt      = fromIntegral dt0 / 1000
  (ss,ds) = partition isStationary (worldBodies w)
  ds'     = map (stepPhysicalState dt) ds
