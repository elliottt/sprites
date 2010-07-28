module World where

import Graphics
import Math.Line
import Math.Matrix
import Math.Normalize
import Math.Point
import Math.Polygon
import Render
import Time

import Control.Monad (guard,foldM)
import Data.Maybe (mapMaybe)
import Graphics.Rendering.OpenGL.GL (GLfloat)


gravity :: Vector
gravity  = Point 0 (-0.098)


-- World Bodies ----------------------------------------------------------------

data State = State
  { sVelocity     :: !Vector
  , sAcceleration :: !Vector
  , sMass         :: !GLfloat
  , sFriction     :: !GLfloat
  } deriving Show

data Body
  = BPolygon !Polygon !State
  | BPoint Point State
    deriving Show


stepBody :: GLfloat -> Body -> Body
stepBody dt b =
  case b of
    BPolygon p s -> stepPolygon dt p s
    BPoint p s   -> stepPoint dt p s

type Step p = GLfloat -> p -> State -> Body

stepState :: GLfloat -> State -> State
stepState dt s = s
  { sVelocity = sVelocity s + scaleVector dt (sAcceleration s)
  }

stepPolygon :: Step Polygon
stepPolygon dt p s = BPolygon p' s'
  where
  s' = stepState dt s
  p' = movePolygon (sVelocity s') p

stepPoint :: Step Point
stepPoint dt p s = BPoint p' s'
  where
  s' = stepState dt s
  p' = p + sVelocity s'


-- | The result of a collision.
data Collision = Collision
  { collisionDirection :: !Vector  -- ^ Direction of the collision
  , collisionForce     :: !Vector
  } deriving Show

mkCollision :: Vector -> State -> State -> Collision
mkCollision p s1 s2 = Collision
  { collisionDirection = normalize p
  , collisionForce     = scaleVector (sMass s1 - sMass s2)
                       $ sAcceleration s1 - sAcceleration s2
  }
  where


-- | Given a list of things, return the smallest and the largest.
range :: Ord a => [a] -> Maybe (a,a)
range []       = Nothing
range (z:rest) = loop z z rest
  where
  loop l h []                 = Just (l,h)
  loop l h (a:as) | a < l     = loop a h as
                  | a > h     = loop l a as
                  | otherwise = loop l h as

-- | Test two polygons for bounding circle overlap.
radiusOverlap :: Polygon -> Polygon -> Bool
radiusOverlap p1 p2 = r1 + r2 > d
  where
  d  = distance (polyCenter p1) (polyCenter p2)
  r1 = polyRadius p1
  r2 = polyRadius p2

-- | Check a collision between two polygons by first testing collision between
-- their bounding circles, then by testing for collision using the split axis
-- theorem.
polygonsCollide :: Polygon -> State -> Polygon -> State -> Maybe Collision
polygonsCollide p1 s1 p2 s2 = do
  guard (radiusOverlap p1 p2)
  let c        = normalize (polyCenter p1 - polyCenter p2)
  let clen     = vectorLength c
  let step z e = do
        let axis = perpendicular e
            len  = vectorLength axis
            f p  = abs (axis `dot` p) / len
        (l1,r1) <- range (map f (polyPoints p1))
        (l2,r2) <- range (map f (polyPoints p2))
        let overlap | r1 >= l2  = r1 - l2
                    | l1 <= r2  = r2 - l1
                    | otherwise = -1
        guard (overlap >= 0)
        return (max overlap z)

  overlap <- foldM step 0 (polyEdges p1 ++ polyEdges p2)
  return (mkCollision c s1 s2)

bodiesCollide :: Body -> Body -> Maybe Collision
bodiesCollide b1 b2 =
  case b1 of
    BPolygon p s -> polygonCollides p s b2
    BPoint p s   -> pointCollides p s b2

polygonCollides :: Polygon -> State -> Body -> Maybe Collision
polygonCollides p s b =
  case b of
    BPolygon p' s' -> polygonsCollide p s p' s'
    BPoint p' s'   -> polygonContains p s p' s'

polygonContains :: Polygon -> State -> Point -> State -> Maybe Collision
polygonContains poly s1 p s2 = do
  let p0   = polyCenter poly
  let rlen = distance p0 p
  guard (rlen <= polyRadius poly)

  let step z e = do
        let axis = perpendicular e
        let len  = vectorLength axis
        let f o  = abs (axis `dot` o) / len
        (l,r) <- range [f p0, f p]
        let overlap | r >= pointX axis = r - pointX axis
                    | l <= pointY axis = pointY axis - l
                    | otherwise        = -1
        guard (overlap >= 0)
        return (max overlap z)

  overlap <- foldM step 0 (polyEdges poly)
  return (mkCollision p s1 s2)

pointCollides :: Point -> State -> Body -> Maybe Collision
pointCollides p s1 b =
  case b of

    BPolygon p' s2 -> polygonContains p' s2 p s1

    BPoint p' s2  -> do
      guard (p == p')
      return (mkCollision (sVelocity s1 - sVelocity s2) s1 s2)


-- World Management ------------------------------------------------------------

type World = [Body]

updateState :: (State -> State) -> Body -> Body
updateState f (BPolygon p s) = BPolygon p (f s)
updateState f (BPoint   p s) = BPoint   p (f s)

bodyState :: Body -> State
bodyState (BPolygon _ s) = s
bodyState (BPoint   _ s) = s

resolveCollision :: Collision -> State -> State
resolveCollision c s = s
  { sAcceleration = sAcceleration s
                  + scaleVector (- sMass s) (collisionForce c)
  , sVelocity = scaleVector (vectorLength (sVelocity s)) (collisionDirection c)
  }

mergeCollision :: Collision -> Collision -> Collision
mergeCollision c1 c2 = Collision
  { collisionDirection =
      normalize (collisionDirection c1 + collisionDirection c2)
  , collisionForce = collisionForce c1 + collisionForce c2
  }

bodies :: World -> [(Body,[Body])]
bodies ws = loop ws [] []
  where
  loop []     _  rs = rs
  loop (a:as) bs rs = loop as (a:bs) ((a,as++bs):rs)

resolveCollisions :: World -> IO World
resolveCollisions  = mapM body . bodies
  where
  body (w,ws) = do
    case mapMaybe (bodiesCollide w) ws of
      [] -> return w
      cs -> do
        let c = foldr1 mergeCollision cs
        if sMass (bodyState w) > 0 then print c else return ()
        return (updateState (resolveCollision c) w)


stepWorld :: Interval -> World -> IO World
stepWorld dt w = do
  let dt' = fromIntegral dt / 1000
  map (stepBody dt') `fmap` resolveCollisions w

instance Render Body where
  render (BPolygon p _) = renderPrimitive Lines (render (polyEdges p))
  render (BPoint p _)   = renderPrimitive Points (render p)
