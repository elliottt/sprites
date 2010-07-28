module Physics.Shape (
    -- * Shapes
    Shape
  , center
  , radius
  , distanceBetween

    -- * Construction
  , circle
  , polygon
  , rectangle

    -- * Collision Checking
  , radiusOverlap
  , checkCollision
  ) where

import Math.Line
import Math.Normalize
import Math.Point
import Physics.Vector

import Control.Monad (guard,foldM)
import Graphics.Rendering.OpenGL.GL (GLfloat)


-- Shapes ----------------------------------------------------------------------

data Shape
  = SCircle !Point !GLfloat
  | SPolygon !Point !GLfloat [Point]
    deriving Show

-- | Get the center of a shape.
center :: Shape -> Point
center (SCircle c _)    = c
center (SPolygon c _ _) = c

-- | Get the radius of a shape
radius :: Shape -> GLfloat
radius (SCircle _ r)    = r
radius (SPolygon _ r _) = r

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
  | otherwise    = Just (SPolygon c (maximum radii) ps)
  where
  numVerts = length ps
  c        = sum ps / fromIntegral numVerts
  radii    = map (distance c) ps


rectangle :: Point -> GLfloat -> GLfloat -> Maybe Shape
rectangle c@(Point x y) w h = do
  guard (w > 0 && h > 0)
  let w2 = w / 2
      h2 = h / 2
      tl = Point (x-w2) (y+h2)
      tr = Point (x+w2) (y+h2)
      br = Point (x+w2) (y-h2)
      bl = Point (x-w2) (y-h2)
  return (SPolygon c (distance0 (Point w2 h2)) [tl,tr,br,bl])


-- Collision Checking ----------------------------------------------------------

data Collision = Collision !Vector deriving Show

-- | Check to see if the bounding circles of two shapes overlap.
radiusOverlap :: Shape -> Shape -> Bool
radiusOverlap s1 s2 = radius s1 + radius s2 >= distanceBetween s1 s2

-- | Check for collisions, requiring that the bounding circles of each shape are
-- overlapping.
checkCollision :: Shape -> Shape -> Maybe Collision
checkCollision s1 s2 = do
  guard (radiusOverlap s1 s2)
  checkCollision' s1 s2

-- | Dispatch to more specific collision checking.
checkCollision' :: Shape -> Shape -> Maybe Collision
checkCollision' s1 s2 =
  case (s1,s2) of

    (SCircle c1 r1, SCircle c2 r2) ->
      return (Collision (pointToVector (c1 - c2)))

    (SPolygon c1 r1 ps, SCircle c2 r2) ->
      checkPolygonCircle c1 r1 ps c2 r2

    (SCircle c1 r1, SPolygon c2 r2 ps) -> do
      Collision v <- checkPolygonCircle c2 r2 ps c1 r1
      return (Collision (invertVector v))

    (SPolygon c1 _ ps1, SPolygon c2 _ ps2) ->
      checkPolygonPolygon c1 ps1 c2 ps2


range :: Ord a => [a] -> Maybe (a,a)
range []     = Nothing
range (a:as) = loop a a as
  where
  loop l h (x:xs) | x > h     = loop l x xs
                  | x < l     = loop x h xs
                  | otherwise = loop l h xs
  loop l h []                 = Just (l,h)

overlapSize :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> GLfloat
overlapSize (l1,r1) (l2,r2)
  | l1 < l2   = r1 - l2
  | otherwise = r2 - l1

-- | Check the collision of a polygon and a circle.
checkPolygonCircle :: Point -> GLfloat -> [Point]
                   -> Point -> GLfloat
                   -> Maybe Collision
checkPolygonCircle c1 r1 ps c2 r2 = do
  let ray                 = Line c1 c2
      step e@(Line p _) = do
        let axis   = perpendicular e
            alen   = distance0 axis
            proj x = abs (axis `dot` x) / alen
            p'     = proj p
        (l,r) <- range [proj c1, proj c2]
        guard (p' >= r || p' <= l)

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
        let o = overlapSize p1 p2
        guard (o >= 0)
        return (max z o)

  overlap <- foldM step 0 axes
  let dir = normalize (pointToVector (c1 - c2))
  return (Collision (scaleVector overlap dir))
