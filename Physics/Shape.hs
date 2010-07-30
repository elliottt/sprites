module Physics.Shape (
    -- * Shapes
    Shape
  , center
  , distanceBetween
  , direction

    -- * Construction
  , circle
  , polygon
  , rectangle
  , triangle
  ) where

import Math.Line
import Math.Normalize
import Math.Point
import Math.Utils
import Physics.AABB
import Physics.Body
import Physics.Collision
import Physics.Vector

import Control.Monad (guard,foldM)

import Debug.Trace


-- Shapes ----------------------------------------------------------------------

data Shape
  = SCircle !Point !GLfloat
  | SPolygon !Point [Point]
    deriving Show

instance Physical Shape where
  boundingBox = shapeAABB
  moveBy      = moveShape
  position    = center

instance Collides Shape where
  collides = checkCollision

-- | Get the center of a shape.
center :: Shape -> Point
center (SCircle c _)  = c
center (SPolygon c _) = c

-- | Get the distance between the centers of two shapes.
distanceBetween :: Shape -> Shape -> GLfloat
distanceBetween s1 s2 = distance (center s1) (center s2)

-- | Return the vector that points from shape 1 to shape 2.
direction :: Shape -> Shape -> Vector
direction s1 s2 = normalize (pointToVector (center s2 - center s1))


-- Construction ----------------------------------------------------------------

-- | Given a point and a radius, make a circle.
circle :: Point -> GLfloat -> Shape
circle  = SCircle

-- | Create a polygon, given a list of points.  If there aren't enough points to
-- make a triangle, return Nothing.  In the future, check to see that the points
-- given form a convex polygon, and if not, return Nothing.
polygon :: [Point] -> Maybe Shape
polygon ps
  | numVerts < 3 = Nothing
  | otherwise    = Just (SPolygon c ps)
  where
  numVerts = length ps
  c        = sum ps / fromIntegral numVerts


rectangle :: Point -> GLfloat -> GLfloat -> Maybe Shape
rectangle c@(Point x y) w h = do
  guard (w > 0 && h > 0)
  let w2 = w / 2
      h2 = h / 2
      tl = Point (x-w2) (y+h2)
      tr = Point (x+w2) (y+h2)
      br = Point (x+w2) (y-h2)
      bl = Point (x-w2) (y-h2)
  return (SPolygon c [tl,tr,br,bl])

triangle :: Point -> Point -> Point -> Maybe Shape
triangle p1 p2 p3 = polygon [p1,p2,p3]


-- Movement --------------------------------------------------------------------

-- | Move a shape by a vector.
moveShape :: Vector -> Shape -> Shape
moveShape (Vector x y) s =
  case s of
    SCircle c r   -> SCircle (c+p) r
    SPolygon c ps -> SPolygon (c+p) (map (+p) ps)
  where
  p = Point x y


-- Collision Checking ----------------------------------------------------------

-- | Construct the AABB for a shape.
shapeAABB :: Shape -> AABB
shapeAABB s =
  case s of
    SCircle (Point x y) r -> AABB (Point (x-r)  (y+r)) (Point (2*r) (2*r))
    SPolygon _ ps         -> AABB (Point x y) (Point (x'-x) (y-y'))
      where
      proj_x p    = p `dot` Point 1 0
      Just (x,x') = range (map proj_x ps)
      proj_y p    = p `dot` Point 0 1
      Just (y',y) = range (map proj_y ps)

-- | Check for collisions.
checkCollision :: Shape -> Shape -> Maybe Collision
checkCollision s1 s2 =
  case (s1,s2) of

    (SCircle c1 _, SCircle c2 _) ->
      return (Collision (pointToVector (c1 - c2)))

    (SPolygon c1 ps, SCircle c2 r2) ->
      checkPolygonCircle c1 ps c2 r2

    (SCircle c1 r1, SPolygon c2 ps) -> do
      Collision v <- checkPolygonCircle c2 ps c1 r1
      return (Collision (invertVector v))

    (SPolygon c1 ps1, SPolygon c2 ps2) ->
      checkPolygonPolygon c1 ps1 c2 ps2

-- | Check the collision of a polygon and a circle.
checkPolygonCircle :: Point -> [Point]
                   -> Point -> GLfloat
                   -> Maybe Collision
checkPolygonCircle c1 ps c2 r2 = do
  let r0 = distance c1 c2 - r2
  let c3 = c1 + scalePoint r0 (normalize (c2 - c1))
  let step e@(Line p _) = do
        let axis   = normalize (perpendicular e)
            alen   = distance0 axis
            proj x = abs (axis `dot` x) / alen
            p'     = proj p
        (l,r) <- range [proj c1, proj c3]
        -- if the segment (c1,c3) falls to either side of the line then it's
        -- within the polygon
        show (l,r,p') `trace` guard (p' >= r || p' <= l)

  mapM_ step (edges ps)
  let dir = normalize (pointToVector (c1 - c2))
  return (Collision (scaleVector (distance c1 c2) dir))

-- | Check the collision of two polygons.
checkPolygonPolygon :: Point -> [Point]
                    -> Point -> [Point]
                    -> Maybe Collision
checkPolygonPolygon c1 ps1 c2 ps2 = do
  let axes        = map (normalize . perpendicular) (edges ps2 ++ edges ps2)
      step z axis = z `seq` do
        let proj p = axis `dot` p
        p1 <- range (map proj ps1)
        p2 <- range (map proj ps2)
        let o = rangeOverlap p1 p2
        guard (o >= 0)
        return (max z o)

  overlap <- foldM step 0 axes
  let dir = normalize (pointToVector (c1 - c2))
  return (Collision (scaleVector overlap dir))
