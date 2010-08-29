module Physics.World where

import Graphics
import Math.AffinePlane
import Math.Utils
import Physics.AABB
import Physics.Body
import Physics.Shape
import Time

import Control.Monad (guard,when)
import Data.List (partition)
import Data.Maybe (fromMaybe,mapMaybe,catMaybes)
import qualified Data.Set as Set

import Debug.Trace


type Body = PhysicalState

data World = World
  { worldBox         :: !AABB
  , worldBodies      :: [Body]
  , worldGravity     :: Maybe (Vector GLfloat)
  , worldIds         :: [Int]
  }

instance Render World where
  render w = render (worldBodies w)

emptyWorld :: GLfloat -> GLfloat -> World
emptyWorld w h = World
  { worldBox         = AABB (Point 0 h) w h
  , worldBodies      = []
  , worldGravity     = Nothing
  , worldIds         = [1 ..]
  }

-- | Move the world forward
stepWorld :: Interval -> World -> IO World
stepWorld dt0 w = do
  let dt = fromIntegral dt0 / 1000
  w' <- handleCollisions dt w
  return (stepVelocities dt w')

handleCollisions :: GLfloat -> World -> IO World
handleCollisions dt w = do
  let (ss,ds) = partition isStationary (worldBodies w)
  ds' <- collisions ss ds
  let step (b1,rs) = do
        let resolve (b2,cs) = map (resolveCollision w b1 b2) (Set.toList cs)
        case concatMap resolve rs of

          [] -> stepPhysicalState dt b1

          -- calculate the impulse force, and apply it
          vs -> do
            let v = foldl1 (+^) vs
            when (psDebug b1) $ do
              putStr "Impulse: "
              print v
            stepPhysicalState dt (applyImpulse v b1)

  ds'' <- mapM step ds'
  return w { worldBodies = ss ++ ds'' }

-- | Step the velocities of all the bodies in the world.
stepVelocities :: GLfloat -> World -> World
stepVelocities dt w = w
  { worldBodies = map (adjustVelocity dt) (worldBodies w)
  }

-- | Turn a collision into a new velocity.
resolveCollision :: World -> Body -> Body -> Contact -> Vector GLfloat
resolveCollision w b1 b2 c = debug
  where
  n     = conNormal c
  n'    = normalV n
  v     = psVelocity b1
  nv    = n <.> v
  nperp = projAlong v n'
  rest  = min (psRestitution b1) (psRestitution b2)
  v'    = nperp -^ (rest * nv *^ n)

  debug | psDebug b1 = trace (show c)
                     $ trace (show v)
                     $ trace (show v')
                     $ trace (show rest)
                     $ v'
        | otherwise  = v'

-- | Partition out collisions, and static bodies.
collisions :: [Body] -> [Body] -> IO [(Body,[(Body,Set.Set Contact)])]
collisions ss ds = step ss ds []
  where
  step bf []     cs = return cs
  step bf (x:xs) cs = do
    mbs <- mapM (collidePhysicalState x) (bf ++ xs)
    let f (_,b,c) = (b,c)
    step (x:bf) xs ((x,mapMaybe (fmap f) mbs):cs)

addBody :: Body -> World -> World
addBody b w =
  case worldIds w of
    []   -> error "addBody: Out of world ids!"
    i:is -> w
      { worldBodies = b' { psId = i } : worldBodies w
      , worldIds    = is
      }
  where
  b' | psStatic b = b
     | otherwise  = b { psAcceleration = (+^) (psAcceleration b)
                                       $ fromMaybe zero
                                       $ worldGravity w }
