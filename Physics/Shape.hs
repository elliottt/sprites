module Physics.Shape (
    -- * Shapes
    Shape
  , center
  , distanceBetween
  , direction
  , moveShape

    -- * Construction
  , circle
  , polygon
  , rectangle
  , triangle
  ) where

import Graphics
import Math.Line
import Math.Matrix
import Math.Normalize
import Math.Point
import Math.Utils
import Physics.AABB
import Physics.Body
import Physics.Collision
import Physics.Vector

import Control.Monad (guard,foldM,mplus)


-- Shapes ----------------------------------------------------------------------

data Shape
  = SCircle !Point !GLfloat
  | SPolygon !Point [Point]
    deriving Show

instance Render Shape where
  render s =
    case s of

      SCircle (Point x y) r -> renderPrimitive LineLoop $ do
        let p i = vertex2d (x + r * cos i) (y + r * sin i)
        mapM_ p [0, 0.2 .. 2 * pi]

      SPolygon c ps -> do
        renderPrimitive Points (render c)
        let es = zipWith Line ps (drop 1 (cycle ps))
        renderPrimitive Lines (mapM_ render es)

instance Physical Shape where
  boundingBox = shapeAABB
  transform   = transformShape
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

-- | Create a polygon, given a list of points in counter-clockwise order.  If
-- there aren't enough points to make a triangle, return Nothing.  In the
-- future, check to see that the points given form a convex polygon, and if
-- not, return Nothing.
polygon :: [Point] -> Maybe Shape
polygon ps
  | numVerts < 3 = Nothing
  | otherwise    = Just (SPolygon c ps)
  where
  numVerts = length ps
  c        = sum ps / fromIntegral numVerts


-- | A rectangle centered around a point, with the given width and height.
rectangle :: Point -> GLfloat -> GLfloat -> Maybe Shape
rectangle c@(Point x y) w h = do
  guard (w > 0 && h > 0)
  let w2 = w / 2
      h2 = h / 2
      tl = Point (x-w2) (y+h2)
      tr = Point (x+w2) (y+h2)
      br = Point (x+w2) (y-h2)
      bl = Point (x-w2) (y-h2)
  return (SPolygon c [tr,tl,bl,br])

-- | A triangle, with points listed in counter-clockwise order.
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


-- | Transform a shape by a matrix.
transformShape :: Matrix -> Shape -> Shape
transformShape mat s =
  case s of
    -- XXX: transform the radius of the shape as well.
    SCircle c r   -> SCircle (f c) r
    SPolygon c ps -> SPolygon (f c) (map f ps)
  where
  f = transformPoint mat


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

    (SCircle c1 r2, SCircle c2 r1) -> do
      let dist = distance c1 c2
      let dir  = pointToVector (c1 - c2)
      return Collision
        { collisionDirection = dir
        , collisionOverlap   = dist - r1 - r2
        , collisionNormal    = dir
        }

    (SPolygon c1 ps, SCircle c2 r2) ->
      checkPolygonCircle c1 ps c2 r2

    (SCircle c1 r1, SPolygon c2 ps) -> do
      k <- checkPolygonCircle c2 ps c1 r1
      return k
        { collisionDirection = invertVector (collisionDirection k)
        }

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
        guard (p' >= r || p' <= l)

  mapM_ step (edges ps)
  let dist = distance c1 c2
  let dir  = normalize (pointToVector (c1 - c2))
  return Collision
    { collisionDirection = scaleVector dist dir
    , collisionOverlap   = dist
    , collisionNormal    = Vector 0 0
    }

-- | Check the collision of two polygons.
checkPolygonPolygon :: Point -> [Point]
                    -> Point -> [Point]
                    -> Maybe Collision
checkPolygonPolygon c10 ps10 c20 ps20 = do
  k1 <- check c10 ps10 c20 ps20
  k2 <- check c20 ps20 c10 ps10
  if collisionOverlap k1 <= collisionOverlap k2
     then return k1
     else return k2
       { collisionDirection = invertVector (collisionDirection k2)
       , collisionNormal    = invertVector (collisionNormal k2)
       }

  where

  check c1 ps1 c2 ps2 = do
    let step s@(z,_) edge = z `seq` do
          let axis   = normalize (perpendicular edge)
          let proj p = axis `dot` p
          p1 <- range (map proj ps1)
          p2 <- range (map proj ps2)
          let o = rangeOverlap p1 p2
          guard (o >= 0)
          if o < z then return (o,axis) else return s
    (overlap,Point x y) <- foldM step (1000000,Point 0 0) (edges ps1)
    let dir = normalize (pointToVector (c1 - c2))
    return Collision
      { collisionDirection = dir
      , collisionOverlap   = overlap
      , collisionNormal    = Vector x y
      }
